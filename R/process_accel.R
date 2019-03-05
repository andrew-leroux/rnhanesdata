#' Process NHANES 2003-2004 and 2005-2006 accelerometry data
#'
#' @description
#' This function processes the raw NHANES 2003-2004 and 2005-2006 accelerometry data
#' as provided by the NHANES study on the CDC website.
#' Note that due to the large file size of the unzipped .xpt files, this function uses a non-trivial
#' amount of RAM (~12 GB peak). To avoid crashing your computer when running, please ensure you have
#' enough RAM available.
#'
#' @param names_accel_xpt A character vector of names for the zipped raw files. These should be of the form PAXRAW_\* where \* corresponds to the
#' letter of the alphabet indicating which "wave" the data is from. For example, PAXRAW_C and PAXRAW_D correspond to the 2003-2004 and 2005-2006 waves
#' accelerometery data, respectively. A vector containing PAXRAW_C and PAXRAW_D is the default and will process both 2003-2004 and 2005-2006 waves data.
#'
#' @param local Logical value indicating whether the zippped raw .xpt accelerometry files are stored locally.
#' If FALSE, will download the data into a temporary file from the CDC website and process the data. If TRUE, localpath must be specified and
#' the zipped data will be sourced locally. Defaults to FALSE.
#'
#' @param localpath Character string indicating where the locally zipped raw .xpt files are. If local=TRUE, then localpath must be a valid local directory.
#'
#' @param urls Character vector provides the website URLs where the NHANES accelerometry data can be downloaded. The default contains the URLs which will directly download the data frome
#' the CDC's website. This argument, if specified, must be the same length as the names_accel_xpt argument. Downloading the data through R is often slower than downloading the data outside of R.
#' See the examples section below for how to download and process the data directly from the CDC.
#'
#' @param zipped Logical scalar indicating whether the physical activity files are in the zipped format downloaded directly from the CDC website (.ZIP).
#' If local=FALSE and the data are downloaded from the CDC's website, this argument is ignored. Note that if the data are saved locally, processing speed is
#' substantially increased by unzipping before calling the process_accel function.
#'
#' @param check_data logical value indicating whether to perform some checks of the data. If TRUE, the function will incur additional processing time.
#' The NHANES 2003-2006 data have been tested and already passed these checks. Defaults to FALSE.
#'
#' @details
#'
#' This function takes the long format of the NHANES 2003-2006 accelerometry data and transforms it into the 1440+ format, with one row per participant-day, and
#' 7 rows per participant. Although process_accel will try to process any ".xpt" or ".ZIP" file which follows the NHANES accelerometry naming convention, it has only been tested
#' on the NHANES 2003-2006 waves' accelerometry data. As future NHANES accelerometry data are released, we intend to verify that process_accel
#' will correctly transform the newly released data into our 1440+ format.
#' The function documentation, and, if necessary the function itself, will be updated as needed going forward.
#'
#' If the data are directly downloaded from the CDC website, the raw data will be downloaded to a temporary folder and then deleted once it's been read into R.
#'
#' @return
#'
#' This function will return a list with number of elements less than or equal to the number of waves of data specified by the names_accel_xpt
#' argument. The exact number of elements returned will depend on whether all files specified by the user are found in either: 1) the local directory
#' indicated by the localpath argument; or 2) downloadable from the website(s) indicated by the "urls" argument. Each element of the list returned is a data frame
#' with columns:
#'
#' \itemize{
#'    \item{SEQN:}{ Unique subject identifier}
#'    \item{PAXCAL:}{ Device calibration.
#'    Was the device calibrated when it was returned by the participant? 1 = Yes, 2 = No, 9 = Don't Know.
#'    Any individuals with either 2 or 9 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{PAXSTAT:}{ Data reliability status flag. 1 = Data deemed reliable, 2 = Data reliability is questioable.
#'    Any individuals with 2 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{SDDSRVYR:}{ Variable indicating which wave of the NHANES study this data is associated with. For example,
#'    SDDSRVYR = 3 corresponds to the 2003-2004 wave and SDDSRVYR = 4 corresponds to the 2005-2006 wave.}
#'    \item{WEEKDAY:}{ Day of the week: 1 = Sunday, 2 = Monday, 3 = Tuesday, 4 = Wednesday, 5 = Thursday, 6 = Friday, 7 = Saturday.}
#'    \item{MIN1-MIN1440:}{ Activity count corresponding to each minute of the day. For example, MIN1 is the activity count for 00:00-00:01. }
#' }
#'
#'
#' @examples
#' \dontrun{
#' library("rnhanesdata")
#' ## download and process the data directly from the cdc
#' ## the first element of accel_ls corresponds to PAXINTEN_C and
#' ## the second element of accel_ls corresponds to PAXINTEN_D
#' accel_ls <- process_accel(names_accel_xpt = c("PAXRAW_C","PAXRAW_D"),
#'                           local=FALSE,
#'                           urls=urls=c("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PAXRAW_C.ZIP",
#'                                       "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAXRAW_D.ZIP")
#'                          )
#'
#' ## check to see that the data processed using the process_accel function
#' ## are identical to the processed data included in the package
#' identical(accel_ls[[1]], PAXINTEN_C)
#' identical(accel_ls[[2]], PAXINTEN_D)
#' }
#'
#' @importFrom haven read_xpt
#'
#' @importFrom utils write.csv unzip download.file

#'
#' @export
process_accel <- function(names_accel_xpt = c("PAXRAW_C","PAXRAW_D"),
                          local=FALSE,localpath=NULL, urls=NULL,
                          zipped=TRUE, check_data=FALSE){

        if(local & is.null(localpath)){
                stop("If specifying local = TRUE, must specify localpath as a local directory where the (un)zipped data files are saved.")
        }
        if(local){
                if(!dir.exists(localpath)){
                    stop("localpath must be a valid local directory path.")
                }
                if(any(!paste0(names_accel_xpt,".xpt") %in% list.files(localpath))  & !zipped){
                    stop(paste0("One or more of ", paste0(names_accel_xpt,".xpt"), " not found in localpath directory."))
                }
                if(any(!paste0(names_accel_xpt,".ZIP") %in% list.files(localpath))  & zipped){
                    stop(paste0("One or more of ", paste0(names_accel_xpt,".ZIP"), " not found in localpath directory."))
                }
        }
        if(any(!grepl("^PAXRAW_[A-Z]$", names_accel_xpt))){
                stop("Raw accelerometer data must follow the NHANES naming convention of PAXRAW_* where * denotes the letter corresponding the NHANES wave")
        }
        if(!local){
                if(any(is.null(urls), length(urls) != length(names_accel_xpt))){
                        stop("If the data is not locally stored urls must be a vector of the same length as names_accel_xpt.")
                }
        }


        out.name <- gsub("PAXRAW", "PAXINTEN", names_accel_xpt)
        ret <- c()

        pb <- txtProgressBar(min=0, max=length(names_accel_xpt), style=3)

        for(i in seq_along(names_accel_xpt)){
                datapath <- c()
                xpt_nm <- tolower(paste0(names_accel_xpt[i],".xpt"))
                # if local=FALSE, download the data directly from the link provided in URLS
                if(!local){
                        datapath <- system.file("extdat",package="rnhanesdata")
                        temp     <- tempfile()
                        temp_xpt <- tempfile()

                        download.file(urls[i], temp)
                        unzip(temp, xpt_nm, exdir=temp_xpt)

                        sim.data <- read_xpt(file.path(temp_xpt, xpt_nm))

                        unlink(c(temp,temp_xpt), recursive=TRUE)

                }
                if(local){
                        datapath <- localpath
                        if(zipped){
                            sim.data <- read_xpt(unzip(file.path(datapath, paste0(names_accel_xpt[i],".ZIP")), xpt_nm, exdir=datapath))
                        } else {
                            sim.data <- read_xpt(file.path(datapath, xpt_nm))
                        }
                }


                uid <- as.character(unique(sim.data$SEQN))

                if(check_data){
                    ## read_xpt reads all variables in as numeric, check for truncation on reading in
                    stopifnot(all(nchar(uid)==5))

                    ## get all indices for all subjects
                    sp_n_inx <- split(1:nrow(sim.data), sim.data$SEQN)

                    ## check that all subjects have 7 or fewer days
                    sp_n_ln <- vapply(sp_n_inx, length, numeric(1))
                    if(!all(sp_n_ln <= 7*1440)) stop("Some participants have more than 7 days of data")

                    ## check that within a subject all data are sequential (i.e. no intermittent missing minutes)
                    ## AND that all these numbers are equal to the column PAXN
                    sp_n <- lapply(sp_n_inx, function(x){
                                tmp <- sim.data[x,]
                                tmp$n <- (tmp$PAXMINUT + 60*tmp$PAXHOUR) + (rep(0:6, each=1440)*1440)[1:length(x)]
                                any(diff(tmp$n) != 1) | any(tmp$n+1 != tmp$PAXN)
                            })
                    if(!all(!unlist(sp_n))) stop("There is some intermittent missingness within participants. Please check the data source.")
                }



                ## create empty data frame with full 7 days of data for each subject (10,080 rows/subject)
                n    <- length(uid)
                seqn <- rep(uid,each=10080)
                paxn <- rep(c(1:10080),n)
                full.list <- data.frame(SEQN=seqn,PAXN=paxn)
                rm(list=c("n","seqn","paxn"))


                inx <- match(paste0(full.list$SEQN, "_", full.list$PAXN),
                             paste0(sim.data$SEQN, "_", sim.data$PAXN))
                full.na <- cbind(full.list, sim.data[inx,-c(1,5)])


                rm(list=c("full.list","inx"))


                ## create id and day of the week variables to fill in
                ## note: this assumes PAXCAL/PAXSTAT do not change from with subjects
                ## this was verified outside of this function for the 2003-2004 and 2005-2006 waves
                u_inx  <- which(!duplicated(sim.data$SEQN))
                u_data <- sim.data[u_inx,c('SEQN','PAXCAL','PAXSTAT','PAXDAY'), drop=FALSE]

                cal  <- as.integer(rep(u_data$PAXCAL,each=7))
                stat <- as.integer(rep(u_data$PAXSTAT,each=7))

                rm(list=c("u_inx"))

                weekday <- rep(NA, length(uid)*7)
                inx_cur <- 1
                for(k in seq_along(uid)){
                        d <- u_data$PAXDAY[k]
                        if (d==1) {x<-c(1:7)}
                        else if (d==2) {x<-c(2:7,1)}
                        else if (d==3) {x<-c(3:7,1:2)}
                        else if (d==4) {x<-c(4:7,1:3)}
                        else if (d==5) {x<-c(5:7,1:4)}
                        else if (d==6) {x<-c(6:7,1:5)}
                        else if (d==7) {x<-c(7,1:6)}
                        weekday[inx_cur:(inx_cur+6)] <- x
                        inx_cur <- inx_cur + 7
                }
                rm(list=c("k","x","inx_cur","sim.data","u_data"))

                id2       <- as.integer(rep(uid,each=7))
                idweekday <- data.frame(SEQN=id2,PAXCAL=cal,PAXSTAT=stat,WEEKDAY=as.integer(weekday),
                                        SDDSRVYR=as.integer(c(1:26)[LETTERS == substr(names_accel_xpt[i], nchar(names_accel_xpt[i]), nchar(names_accel_xpt[i]))]),
                                        stringsAsFactors = FALSE)
                rm(list=c("weekday","id2","uid"))

                col.name <- paste0("MIN",1:1440)

                pax      <- full.na$PAXINTEN
                pax.wide <- data.frame(matrix(as.integer(pax),ncol=1440,byrow=T))
                colnames(pax.wide)<-col.name


                ret[[out.name[i]]] <- data.frame(idweekday,pax.wide,stringsAsFactors = FALSE)

                rm(list=c("pax","pax.wide","col.name","idweekday"))

                setTxtProgressBar(pb, i)
        }
        ret
}
















#' Process wear/non-wear flags for NHANES 2003-2004 and 2005-2006 accelerometry data
#'
#' @description
#'
#' This function creates wear/non-wear flag matrices for processed NHANES 2003-2004 and 2005-2006 accelerometry data.
#' The underlying algorithm for estimating wear/non-wear flags is implemented in the \code{\link{weartime}} function from the accelerometry package.
#'
#'
#'
#' @param x A list with each element corresponding to a data matrix of activity counts in the 1440+ format with 7 rows per individual. Each element of x should be named, and each
#' name should correspond to the naming convention used by the output of \code{\link{process_accel}}.
#' For example, the processed accelerometry data corresponding to the 2003-2004 wave should be named "PAXINTEN_C".
#' The data should be sorted by participant (SEQN) and then in descending order chronologically. If days are not ordered chronologically and distinct_days=FALSE,
#' then the resulting wear/non-wear flag output may not be accurate.
#' The output of \code{\link{process_accel}} can be fed directly to this argument. See examples.
#'
#' @param days_distinct Logical value indicating whether days should be treated as distinct time series within participants. If TRUE, then
#' subjects' wear status at 11:59PM does not affect their wear status at 00:01AM the next morning. Defaults to FALSE, this is generally recommended.
#'
#' @param window Numeric value indicated the size of the moving window used to assess non-wear in minutes. Defaults to 90 minutes.
#' See \code{\link{weartime}} for more details.
#'
#' @param tol maximum number of minutes with counts greater than 0 within the a non-wear interval.
#' See \code{\link{weartime}} for more details.
#'
#' @param tol_upper maximium activity count for any minute within the window
#' Defaults to 99. That is, for a given minute, if the window contains any minutes with
#' activity counts greater than tol.upper, this minute is considered "wear".
#' See \code{\link{weartime}} for more details.
#'
#'
#' @param ... aditional arguments to be passed to \code{\link{weartime}}.
#'
#'
#' @details
#'
#' There are many way to estimate non-wear periods in accelerometry data. Fundamentally, they all involve finding extended periods of implausibly low activity.
#' However, there is no one perfect algorithm, and what qualifies as "implausible" is device-, placement-, and population-dependent. Here, we use the algorithm
#' implemented by default in the \code{\link{accelerometry}} package via the \code{\link{weartime}} function. This algorithm is
#'  similar to the algorithm used in Troiano et. al (2008).
#'
#' There are a number of parameters the algirothm implemented in \code{\link{weartime}} uses to control how aggressive non-wear time identification is.
#' By making the algorithm more agressive (decreasing window size, increasing tolerance for non-zero activity counts), one increases the likelihood of false positives.
#' Conversely, making the algorithm less agressive increases the likelihood of false negatives. By default we use a fairly conservative window size of 90 minutes.
#'
#'
#'
#' @return
#'
#' The function process_flags returns a list with number of elemnts equal to the number of elements in the object supplied to the "x" argument.
#' Each element of the list returned is a dataframe that mirrors the format of dataframes returned from the \code{\link{process_accel}} function, but instead
#' with the columns conveying activity count data replaced with 0/1 indicators for estimated periods of non-wear.
#' More specifically, each element is a data frame with the following columns
#'
#' \itemize{
#'    \item{SEQN:} {Unique subject identifier}
#'    \item{PAXCAL:}{ Device calibration.
#'    Was the device calibrated when it was returned by the participant? 1 = Yes, 2 = No, 9 = Don't Know.
#'    Any individuals with either 2 or 9 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{PAXSTAT:}{ Data reliability status flag. 1 = Data deemed reliable, 2 = Data reliability is questioable.
#'    Any individuals with 2 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{SDDSRVYR:}{ Variable indicating which wave of the NHANES study this data is associated with. For example,
#'    SDDSRVYR = 3 corresponds to the 2003-2004 wave and SDDSRVYR = 4 corresponds to the 2005-2006 wave.}
#'    \item{WEEKDAY:}{ Day of the week: 1 = Sunday, 2 = Monday, 3 = Tuesday, 4 = Wednesday, 5 = Thursday, 6 = Friday, 7 = Saturday.}
#'    \item{MIN1-MIN1440:}{ Wear/Non-wear flag corresponding to each minute of the day. These columns can take on the following 3 values
#'              \itemize{
#'              \item{0:}{ A value of 0 indicates that a particular minute is determined to be "non-wear"}
#'              \item{1:}{ A value of 1 indicated that a particular minute is determined to be "wear"}
#'              \item{NA:}{ A value of NA indicates that a particular minute was missing data in the activity count data matrix used to create this set
#'              of wear/non-wear flags}
#'              }
#'     For example, a value of 0 in the column MIN1 indicates that during the time period 00:00-00:01, it was estimated that the device was not worn.}
#' }
#'
#'
#' @examples
#' \dontrun{
#' library("rnhanesdata")
#' ## In the interest of reducing computation time for this example,
#' ## we use the already processed accelerometry data
#' accel_ls <- list("PAXINTEN_C" = PAXINTEN_C, "PAXINTEN_D" = PAXINTEN_D)
#' flags_ls <- process_flags(x=accel_ls)
#'
#' ## Check to see that these processed flags are identical to
#' ## those provided in the package
#' identical(flags_ls$Flags_C, Flags_C)
#' identical(flags_ls$Flags_D, Flags_D)
#' }
#'
#' @references
#'
#' \itemize{
#'    \item{Choi, Leena et al. “Validation of Accelerometer Wear and Nonwear Time Classification Algorithm.”
#'          Medicine and science in sports and exercise 43.2 (2011): 357–364. PMC. Web. 10 Oct. 2018.}
#'    \item{National Cancer Institute. Risk factor monitoring and methods: SAS programs for analyzing NHANES 2003-2004 accelerometer data.
#'          Available at: http://riskfactor.cancer.gov/tools/nhanes_pam. Accessed Oct. 10, 2018.}
#'    \item{Troiano RP, Berrigan D, Dodd KW, Masse LC, Tilert T, Mcdowell M: Physical activity in the United States measured by accelerometer.
#'          Med Sci Sports Exerc 2008; 40: 181-188.}
#' }
#'
#' @importFrom accelerometry weartime
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
process_flags <- function(x, days_distinct=FALSE, window=90L, tol=2L, tol_upper=99L, ...){
        names_accel_xpt <- names(x)
        if(any(!grepl("^PAXINTEN_[A-Z]$", names_accel_xpt)) | is.null(names_accel_xpt)){
                stop("x must be a list with each element corresponding to PAXINTEN_* where * denotes the letter corresponding the NHANES wave")
        }
        out.name <- paste0("Flags_", substr(names_accel_xpt, nchar(names_accel_xpt), nchar(names_accel_xpt)))

        ret <- c()


        pb <- txtProgressBar(min=0, max=length(names_accel_xpt), style=3)
        for(i in seq_along(names_accel_xpt)){
                full_data <- x[[i]]

                if(any(table(full_data$SEQN) != 7)){
                        stop(paste0("Activity data ", names_accel_xpt[i], " does not have 7 rows of data per subject. Please double check Activitiy count data."))
                }

                if(any(!paste0("MIN",1:1440) %in% colnames(full_data))){
                        stop(paste0("Activity data ", names_accel_xpt[i], " does not have 1440 minutes of data. Please double check Activitiy count data."))
                }
                activity_data <- as.matrix(full_data[,paste0("MIN",1:1440)])
                activity_data[is.na(activity_data)] = 0

                WMX <- matrix(NA,nrow = nrow(activity_data), ncol = ncol(activity_data))

                uid <- unique(full_data$SEQN)

                for(j in 1:length(uid)){
                        inx_i           <- which(full_data$SEQN == uid[j])
                        activity_data_i <- as.vector(t(activity_data[inx_i,]))
                        wt              <- weartime(activity_data_i, window = window, tol = tol,
                                                    tol_upper = tol_upper, days_distinct=days_distinct, ...)
                        WMX[inx_i,] <- matrix(wt, ncol=1440, nrow=length(inx_i), byrow=TRUE)
                        rm(list=c("wt","inx_i","activity_data_i"))
                }



                ## recombine data
                out = data.frame(full_data[,-which(colnames(full_data)%in%paste0("MIN",1:1440)),drop=FALSE], WMX,
                                 stringsAsFactors = FALSE)
                names(out) = names(full_data)

                ## put NAs back where they belong
                out[is.na(full_data)] = NA

                ret[[out.name[i]]] <- out

                rm(list=c("out","full_data","WMX","uid","activity_data"))

                setTxtProgressBar(pb, i)

        }

        ret

}










#' Process mortality data for NHANES 2003-2004 and 2005-2006 waves
#'
#' @description
#' This function creates a clean mortality dataset which can be combined with data from the
#' NHANES 2003-2004/2005-2006 waves.
#'
#' @param waves Character vector indicating the waves . Defaults to a vector with "C" and "D", corresponding to the 2003-2004 and 2005-2006 waves.
#'
#' @param mort_release_yr Nuemric value indicating the year associated with the raw mortality data to be processed. The default, 2011, corresponds to the
#' most recent raw mortality data included in the data package.
#'
#' @param localpath Character scalar describing the location where the raw data are stored.
#' If NULL, the funciton will look in pacakge data directory for the requested raw mortality data.
#' Defaults to NULL.
#'
#'
#' @details
#'
#' As of writing, this function has only been tested on the 2011 release for the 2003-2004 and 2005-2006 NHANES mortality data.
#' The raw data comes in the form of a vector of strings, with each string associated with on participant.
#' Assuming mortality releases for other waves use the same format, this function.
#' As future mortality data are released, we will update the package with both the processed and raw mortality data for the NHANES 2003-2006 waves.
#' If necessary, we will modify the code to be able to process all releases of the mortality data for 2011 and beyond.
#' The documentation here will be updated as we confirm future mortality data releases are processed correctly using this function.
#'
#' @return
#'
#'
#' This function will return a list with number of elements less than or equal to the number of waves of data specified by the "waves"
#' argument. The exact number of elements returned will depend on whether all files specified by the user are found in either: 1) the local directory
#' indicated by the localpath argument; or available in the data package. Each element of the list returned is a data frame
#' with columns:
#'
#' \itemize{
#'    \item{SEQN:}{ Unique subject identifier}
#'    \item{eligstat:}{ Eligibility status for mortality follow-up
#'          \itemize{
#'                  \item{1:}{ Eligible}
#'                  \item{2:}{ Under age 18, not available for public release}
#'                  \item{3:}{ Ineligible}
#'          }
#'    }
#'    \item{mortat:}{ Indicator for whether participant was found to be alive or deceased at follow-up time given by
#'    permth_exm and permth_int
#'          \itemize{
#'                  \item{0:}{ Assumed alive}
#'                  \item{1:}{ Assumed deceased}
#'                  \item{NA:}{ Under age 18, not available for public release or ineligible for mortality follow-up}
#'          }
#'    }
#'    \item{permth_exm:}{ Time in months from the mobile examination center (MEC) assessment where mortality was assessed.}
#'    \item{permth_int:}{ Time in months from the household interview where mortality was assessed.}
#'    \item{ucod_leading:}{ Underlying cause of death recode from UCOD_113 leading causes where available. Specific causes:
#'            \itemize{
#'                  \item{001:}{ Diseases of the heart (I00-I09, I11, I13, I20-I51)}
#'                  \item{002:}{ Malignant neoplasms (C00-C97)}
#'                  \item{003:}{ Chronic lower respiratory diseases (J40-J47)}
#'                  \item{004:}{ Accidents (unintentional injuries) (V01-X59, Y85-Y86)}
#'                  \item{005:}{ Cerebrovascular diseases (I60-I69)}
#'                  \item{006:}{ Alzheimer's disease (G30)}
#'                  \item{007:}{ Diabetes mellitus (E10-E14)}
#'                  \item{008:}{ Influenza and pneumonia (J09-J18)}
#'                  \item{009:}{ Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)}
#'                  \item{010:}{ All other causes (residual)}
#'                  \item{NA:}{ Ineligible, under age 18, assumed alive or no cause data}
#'            }
#'      }
#'      \item{diabetes_mcod:}{ diabetes flag from multiple cause of death (mcod)}
#'      \item{hyperten_mcod:}{ hyperten flag from multiple cause of death (mcod)}
#'      \item{mortscrce_ndi:}{ mortality source: NDI match}
#'      \item{mortscrce_ssa:}{ mortality source: SSA information}
#'      \item{mortscrce_cms:}{ mortality source: CMS information}
#'      \item{mortscrce_dc:}{ mortality source: death certificate match}
#'      \item{mortscrce_dcl:}{ mortality source: data collection}
#'
#' }
#'
#' @examples
#' library("rnhanesdata")
#'
#' ## process NHANES mortality data using the raw mortality data release from 2011 that comes
#' ## with the package
#' mort_ls <- process_mort()
#'
#' ## verify that this yields identical results to the processed data included in the package
#' identical(mort_ls$Mortality_2011_C, Mortality_2011_C)
#' identical(mort_ls$Mortality_2011_D, Mortality_2011_D)
#'
#'
#' @references
#'
#'    National Center for Health Statistics. Office of Analysis and Epidemiology, Public-use Linked Mortality File, 2015.
#'    Hyattsville, Maryland. (Available at the following address: http://www.cdc.gov/nchs/data_access/data_linkage/mortality.htm
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#'
#' @export
process_mort <- function(waves=c("C","D"),
                         mort_release_yr=2011,
                         localpath=NULL){

        if(!all(waves %in% LETTERS)) stop("One or more waves invalid. waves must be identified as a captial letter A-Z.")
        if(!is.numeric(mort_release_yr)) stop("mort_release_yr must be numeric")


        waves <- sort(waves)

        waves_num  <- vapply(waves, function(x) seq(1999, 2049, by=2)[which(LETTERS == x)], numeric(1))
        waves_mort <- paste0("NHANES_",waves_num,"_",waves_num+1,"_MORT_",mort_release_yr,"_PUBLIC.dat")

        if(is.null(localpath)) {
                filepath <- file.path(system.file("extdat","mort",package="rnhanesdata"), waves_mort)
        } else {
                filepath <- file.path(localpath, waves_mort)
        }
        dne      <- !file.exists(filepath)
        if(any(dne)){
                stop(cat("Specified files: \n ",
                            paste0(filepath[which(dne)],collapse=", \n "),
                            "\n not found. Please double check data directory and/or localpath specification."))
        }

        ret <- c()
        pb <- txtProgressBar(min=0, max=length(waves_mort), style=3)
        for(i in seq_along(waves_mort)){
                raw.data = readLines(filepath[i])

                N = length(raw.data)

                seqn = NULL
                eligstat = NULL
                mortstat = NULL
                causeavl = NULL
                ucod_leading = NULL
                diabetes = NULL
                hyperten = NULL

                permth_int = NULL
                permth_exm = NULL
                mortsrce_ndi = NULL
                mortsrce_cms = NULL
                mortsrce_ssa = NULL
                mortsrce_dc = NULL
                mortsrce_dcl= NULL


                for (j in 1:N){

                        seqn = c(seqn,substr(raw.data[j],1,5))
                        eligstat = c(eligstat,as.numeric(substr(raw.data[j],15,15)))
                        mortstat = c(mortstat,as.numeric(substr(raw.data[j],16,16)))
                        causeavl = c(causeavl,as.numeric(substr(raw.data[j],17,17)))
                        ucod_leading = c(ucod_leading,substr(raw.data[j],18,20))
                        diabetes = c(diabetes,as.numeric(substr(raw.data[j],21,21)))
                        hyperten = c(hyperten,as.numeric(substr(raw.data[j],22,22)))

                        permth_int = c(permth_int,as.numeric(substr(raw.data[j],44,46)))
                        permth_exm = c(permth_exm,as.numeric(substr(raw.data[j],47,49)))

                        mortsrce_ndi = c(mortsrce_ndi,substr(raw.data[j],50,50))
                        mortsrce_cms = c(mortsrce_cms,substr(raw.data[j],51,51))
                        mortsrce_ssa = c(mortsrce_ssa,substr(raw.data[j],52,52))
                        mortsrce_dc = c(mortsrce_dc,substr(raw.data[j],53,53))
                        mortsrce_dcl = c(mortsrce_dcl,substr(raw.data[j],54,54))

                }


                ucod_leading                   <- trimws(ucod_leading)
                ucod_leading[ucod_leading==""] <- NA

                out.name <- paste0("Mortality_", mort_release_yr, "_",  waves[i])
                out <- data.frame("SEQN"=as.integer(seqn),
                                  "eligstat"=as.integer(eligstat),
                                  "mortstat"=as.integer(mortstat),
                                  "causeavl"=as.integer(causeavl),
                                  "permth_exm"=permth_exm,
                                  "permth_int"=permth_int,
                                  "ucod_leading"=ucod_leading,
                                  "diabetes_mcod"=as.integer(diabetes),
                                  "hyperten_mcod"=as.integer(hyperten),
                                  "mortsrce_ndi"=as.integer(mortsrce_ndi),
                                  "mortsrce_cms"=as.integer(mortsrce_cms),
                                  "mortsrce_ssa"=as.integer(mortsrce_ssa),
                                  "mortsrce_dc"=as.integer(mortsrce_dc),
                                  "mortsrce_dcl"=as.integer(mortsrce_dcl),
                                  stringsAsFactors=FALSE)


                ret[[out.name]] <- out

                rm(list=c("out","out.name","seqn","eligstat","mortstat","causeavl","permth_exm","permth_int",
                          "ucod_leading","diabetes","hyperten","j","raw.data",
                          paste0("mortsrce_",c("ndi","cms","ssa","dc","dcl")))
                   )

                setTxtProgressBar(pb, i)
        }

        ret


}








#' Merge non-accelerometry data for NHANES waves
#'
#' @description
#' This function retrieves and merges covariate data
#' from one or more NHANES data files across one or more waves of the study. Variables are merged using the NHANES unique subject identifier (SEQN).
#'
#'
#' @param waves character vector with entries of (capitalized) letter of the alphabet corresponding to the
#' NHANES wave of interest. Defaults to a vector containing "C" and "D" corresponding to the NHANES 2003-2004 and 2005-2006 waves.
#'
#' @param varnames character vector indicating which column names are to be searched for.
#' Will check all .XPT files in located in the directory specified by dataPath. If extractAll = TRUE, then this argument is effectively ignored. Defaults
#' to variables which are required to create the processed data matrices \code{\link{Covariate_C}} and  \code{\link{Covariate_D}}. If "SEQN" is not included in
#' varnames, it will be autmatically added.
#'
#' @param localpath file path where covariate data are saved. Covariate data must be in .XPT format,
#' and should be in their own folder. For example, PAXRAW_C.XPT should not be located in the folder with
#' your covariate files. This will not cause an error, but the code will take much longer to run.
#'
#'
#' @param extractAll logical argument indicating whether all columns of all .XPT files in the search path should be returned.
#' If extractALL = TRUE, all variables from all .XPT files with
#' Defaults to FALSE.
#'
#'
#' @details
#'
#' This function will search all .XPT files which match the NHANES naming convention associated with the
#' character vector supplied to the "waves" argument in the specified data directory
#' (either the "localpath" argument, or the raw NHANES data included in the \code{\link{rnhanesdata}} package).
#' Any file which matches the relevant naming convention AND has "SEQN" as their first column name will be searched for the variables requested in the
#' "varnames" argument.
#'
#' It is recommended that if using the process_covar function to merge variables locally, that the local directory include the demographic dataset for each wave
#' (DEMO_C.XPT and DEMO_D.XPT for the 2003-2004 and 2005-2006 waves, respectively).
#' The reason for this is that without the demographic dataset, there is no guarantee that all
#' participants in a wave will be included in the returned results.
#' If the demographic datasets are not in the directory specified by localpath a warnining will be produced.
#' In addition, it is recommended that the local directory contain only .XPT files associated with NHANES.
#'
#' @return
#'
#'
#' This function will return a list with number of elements equal to the number of waves of data specified by the "waves" argument.
#' The name of each element is Covariate_\* where  \* corresponds to each element of the "waves" argument.
#' If none of the variables listed in the "varnames" arguemnt (and/or SEQN if SEQN was not supplied to the  "varnames" argument)
#' for a particular wave are found, then the element of the returned object will be NULL.
#' If none of the user specified variables are found, but subject identifiers (SEQN) are found, the corresponding elements will still be NULL.
#' See the examples below for illustrations of these scenarios.
#'
#' Most variables in NHANES are measured once per individual. In the event that a user requests a variable which has multiple records for a subject,
#' this function will return the variable in matrix format, with one row per participant and number of columns equal to the number of observations per participant.
#' This matrix is returned within each dataframe using an object with class "AsIs" (See \code{\link{I}} for details).
#' For a concrete example, see the examples below.
#'
#'
#'
#' @examples
#' library("rnhanesdata")
#'
#' ## retrieve default variables
#' covar_ls <- process_covar()
#'
#' ## re-code gender for the both the 2003-2004 and 2005-2006 waves
#' covar_ls$Covariate_C$Gender <- factor(covar_ls$Covariate_C$RIAGENDR, levels=1:2,
#'                                       labels=c("Male","Female"), ordered=FALSE)
#' covar_ls$Covariate_D$Gender <- factor(covar_ls$Covariate_D$RIAGENDR, levels=1:2,
#'                                       labels=c("Male","Female"), ordered=FALSE)
#'
#' ## check that this matches the gender information in the processed data
#' identical(covar_ls$Covariate_C[,c("SEQN","Gender")], Covariate_C[,c("SEQN","Gender")])
#' identical(covar_ls$Covariate_D[,c("SEQN","Gender")], Covariate_D[,c("SEQN","Gender")])
#'
#' ## See the data processing package vignette
#' ## for code to fully reproduce the processed data
#' ## included in the package
#'
#'
#' ## Example where only the participant identifer (SEQN) is found for
#' ## the 2003-2004 and 2005-2006 waves, and no data is found for the 2007-2008 wave.
#' covar_ls2 <- process_covar(waves=c("C","D","E"), varnames=c("ThisIsNotValid"))
#' str(covar_ls2)
#'
#'
#' ## Example of variables with possibly multiple responses per participant.
#' ## These variables correspond to self reported physical activity types:
#' ##   PADACTIV: physical activity type (i.e. basketball, swimming, etc.)
#' ##   PADLEVEL: intensity of activity identified by PADACTIV (moderate or vigorous)
#' ##   PADTIMES: # of times activity identified by PADACTIV was done in the past 30 days
#' ## See the codebook at https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PAQIAF_C.htm#PADTIMES
#' ## for additional descriptions of these variables for the 2003-2004 wave
#' covar_ls3 <- process_covar(waves=c("C","D"), varnames=c("PADACTIV","PADLEVEL","PADTIMES"))
#' str(covar_ls3)
#'
#'
#'
#'
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
process_covar <- function(waves=c("C","D"),
                          varnames = c("SDDSRVYR","WTMEC2YR", "WTINT2YR",
                                       "SDMVPSU", "SDMVSTRA",
                                       "RIDAGEMN", "RIDAGEEX", "RIDRETH1", "RIAGENDR",
                                       "BMXWT", "BMXHT", "BMXBMI", "DMDEDUC2",
                                       "ALQ101", "ALQ110", "ALQ120Q","ALQ120U", "ALQ130", "SMQ020", "SMD030", "SMQ040",
                                       "MCQ220","MCQ160F", "MCQ160B", "MCQ160C",
                                       "PFQ049","PFQ054","PFQ057","PFQ059", "PFQ061B", "PFQ061C", "DIQ010"),
                          localpath=NULL,
                          extractAll=FALSE){

        if(!all(waves %in% LETTERS)) stop("One or more waves invalid. waves must be identified as a captial letter A-Z.")
        stopifnot(length(waves) >= 1)
        stopifnot(is.vector(waves))

        waves <- sort(waves)
        ## add a counter if SEQN is supplied to the varnames argument -- this only affects a message printed to the user on # of matched variables
        cnt   <- ifelse("SEQN" %in% varnames, 1, 0)
        ## ensure ID variable (SEQN) is in search regardless of whether it was included in the varnames argument
        varnames <- unique(c('SEQN',varnames))
        ## find all files of .xpt structure that correspond to the year specified in the data
        if(is.null(localpath)){
                localpath <- system.file(file.path("extdat","covar"),package="rnhanesdata")
        }
        files_full   <- list.files(localpath)

        if(any(!paste0("DEMO_",waves,".XPT") %in% files_full)){
            warning(cat(paste0("One or more demographic files were not found in the data directory (",paste0("DEMO_",waves,".XPT", collapse=", ") , ").
                           There is no guarantee all participants for a particular wave will be included in the returned object.")))
        }


        ret <- rep(list(NULL), length(waves))
        names(ret) <- paste0("Covariate_", waves)
        pb <- txtProgressBar(min=0, max=length(waves), style=3)
        for(i in seq_along(waves)){
                cohort <- waves[i]

                pathExt <- paste0('_', cohort, '.XPT', sep='')
                ## find only those files associated with a particular NHANES wave
                files   <- files_full[substr(files_full, (nchar(files_full) - 5), nchar(files_full)) == pathExt]

                if(length(files) == 0){
                        message(cat(paste0("\n No data associated with wave ", waves[i], " was found. \n")))
                        setTxtProgressBar(pb, i)
                        next
                }

                ## find which files contain the variables requested by the user
                covarMats <- lapply(files, function(x){
                        mat <- try(read_xpt(file.path(localpath,x)))
                        if(inherits(mat, "try-error")) return(NULL)
                        ## expecting SEQN to be first column in all NHANES datasets
                        if(!"SEQN" %in% colnames(mat)[1]) return(NULL)
                        ## if extractAll = FALSE, return only those columns specified in the varnames argument
                        if(!extractAll){
                                if(length(setdiff(intersect(colnames(mat),varnames), "SEQN"))==0) return(NULL)
                                mat <- mat[,colnames(mat)%in%varnames,drop=FALSE]
                        }
                        ## if extractAll = TRUE, return all columns
                        if(!any(is.null(dim(mat)),all(colnames(mat) %in% "SEQN"))) {
                                return(mat)
                        } else {
                                return(NULL)
                        }
                })
                ## remove any matrices with no matches
                covarMats <- covarMats[!vapply(covarMats, is.null,logical(1))]
                matchedNames <- lapply(covarMats, colnames)
                numMatched   <- length(unlist(matchedNames))

                if(numMatched == 0) {
                    message(cat(paste0("\n No variables specified by the varnames argument was found for wave ", waves[i], "\n")))
                    setTxtProgressBar(pb, i)
                    next
                }
                if(numMatched > 0 & !extractAll)  message(
                        cat(paste("\n For", cohort, "cohort,",
                              (numMatched - length(matchedNames) + cnt),
                              'Covariates Found of', (length(varnames)-1 + cnt),'specified.',
                              'Missing covariates:',
                              paste(setdiff(varnames, unlist(matchedNames)),collapse=", ") )))

                ## Merge the covariate data
                ids        <- lapply(covarMats, function(x) x[['SEQN']])
                uids       <- as.integer(sort(unique(unlist(ids))))
                ## check for duplicated SEQN. If there is more than one row per subject, we use a different merging mechanism
                rep_SEQN   <- vapply(ids, function(x) any(duplicated(x)), logical(1))
                rep_inx    <- which(rep_SEQN)
                notrep_inx <- which(!rep_SEQN)

                totalCols <- sum(vapply(covarMats, ncol, numeric(1))) - length(covarMats) + 1
                CovarMat           <- matrix(NA,ncol=totalCols,nrow=length(uids))
                colnames(CovarMat) <- c('SEQN', unlist(sapply(covarMats,function(x) colnames(x)[-1])))
                CovarMat[,'SEQN']  <- uids
                CovarMat <- data.frame(CovarMat)
                invisible(lapply(covarMats[notrep_inx], function(x) CovarMat[,colnames(x)[-1]] <<- x[match(CovarMat$SEQN,x$SEQN),-1,drop=FALSE] ) )

                ## if any of the variabels have multiple observations per id,
                ## convert them to a matrix wide format, then merge
                if(length(rep_inx) != 0) {
                        invisible(lapply(covarMats[rep_inx], function(x){
                                                n_vars <- length(colnames(x))
                                                for(j in 2:n_vars){
                                                        max_reps <- max(table(x$SEQN),na.rm=TRUE)
                                                        mat_tmp  <- matrix(NA, ncol=max_reps, nrow=nrow(CovarMat))

                                                        for(i in 1:nrow(CovarMat)){
                                                                if(!CovarMat$SEQN[i] %in% x$SEQN) next
                                                                ji_inx <- which(x$SEQN == CovarMat$SEQN[i])
                                                                mat_tmp[i,1:length(ji_inx)] <- x[ji_inx,j,drop=TRUE]

                                                                rm(list=c("ji_inx"))
                                                        }
                                                        CovarMat[colnames(x)[j]] <<- I(mat_tmp)
                                                        rm(list=c("max_reps","mat_tmp"))
                                                }
                                        })
                                )
                        message(
                                cat(paste("\n Variables with repeated observations per subject found for the following variables:",
                                      paste(sapply(covarMats[rep_inx], function(x) colnames(x)[-1])  , collapse=","),
                                      "Note that these variables will be stored with class AsIs() objects in resulting data frames. ",
                                      "See ?I for details on AsIs class. \n"))
                                )
                }


                out.name <- paste0("Covariate_", cohort)

                ret[[out.name]] <- CovarMat

                rm(list=c("CovarMat","out.name"))

                setTxtProgressBar(pb, i)
        }

        ret

}



#' Remove days with too few/too much weartime and NHANES data quality flags.
#'
#' @description
#' This function subsets accelerometry data by total wear/non-wear time criteria, as well as
#' NHANES data quality flags.
#'
#' @param act Activity data matrix. Should contain, at a minimum, the columns: SEQN, PAXCAL, PAXSTAT, WEEKDAY, SDDSRVYR. In addition,
#' the activity data matrix should have 1440 columns with MIN1, MIN2, ..., MIN1440.
#'
#' @param flags Wear/non-wear flag matrix. Should contain the same columns as the activity data matrix, in the same order. However, instead of
#' activity counts, the columns MIN1, MIN2, ..., MIN1440 should be binary (0/1) wear/non-wear flags where 1 indicates wear and 0 indicates non-wear.
#'
#' @param threshold_lower Lower limit on the amount of wear-time for a day to be considered good. Defaults to 600 minutes which implies a "good" day has
#' at least 10 hours of weartime.
#'
#' @param rm_PAXSTAT Logical value indicating whether to remove based on the data reliability indicator variable PAXSTAT. Defaults to TRUE.
#'
#' @param rm_PAXCAL Logical value indicating whether to remove based on the accelerometer calibration indicator variable PAXCAL. Defaults to TRUE.
#'
#' @details
#'
#'
#' Fundamentally, all this function does is check to see if a day has a user specified amount  of estimated  total wear time (ignoring timing of wear)
#' and checking for whether any data quality concerns were indicated by NHANES via the PAXSTAT and PAXCAL variables.
#' Because this function doesn't actually use the activity count data, it's not technically necessary to include the activity data matrix. However, forcing
#' the activity data matrix to be included and checking that subject-days are identical between the activity and wear/non-wear flag matrices adds a layer of
#' protection against subsetting or sorting one data matrix but not the other.
#'
#' This function ignores missing data. Missing values to not count toward (or against) wear time.
#'
#'
#' @return
#'
#' This function returns a numeric vector containing the indices of days which were identified as "good". These indices can be used to subset
#' the accelerometry data as desired. An illustration is provided in the examples.
#'
#'
#' @examples
#'
#' library("rnhanesdata")
#'
#' ## remove all days with fewer than 10 hours of wear time in the 2003-2004 accelerometry data
#' ## and exclude all days with data quality/calibration flags
#' data("PAXINTEN_C")
#' data("Flags_C")
#'
#' ## obtain indices for "good" days using the default threshold of at least 10 hours of weartime
#' keep_inx <- exclude_accel(act = PAXINTEN_C, flags = Flags_C)
#' ## subset the accelerometry and flags data using these indices
#' accel_good_C <- PAXINTEN_C[keep_inx,]
#' flags_good_C <- Flags_C[keep_inx,]
#'
#' ## check that all remaining days have at least 10 hours of wear
#' ## and there are no data quality issues as flagged by the PAXSTAT and PAXCAL variables
#' summary(rowSums(flags_good_C[,paste0("MIN",1:1440)], na.rm=TRUE))
#' table(flags_good_C$PAXSTAT)
#' table(flags_good_C$PAXCAL)
#'
#' @references
#'
#'
#' Hart, Teresa L et al. “How Many Days of Monitoring Predict Physical Activity and Sedentary Behaviour in Older Adults?”
#' The International Journal of Behavioral Nutrition and Physical Activity 8 (2011): 62. PMC. Web. 10 Oct. 2018.
#'
#'
#'
#' @export
exclude_accel <- function(act, flags, threshold_lower = 600, rm_PAXSTAT = TRUE, rm_PAXCAL = TRUE){
        stopifnot(all(is.data.frame(act), is.data.frame(flags)))
        stopifnot(all(colnames(act) == colnames(flags)))
        stopifnot(all(c("SEQN","WEEKDAY","PAXSTAT", "PAXCAL", paste0("MIN",1:1440)) %in% colnames(act)))

        act_cols <- which(colnames(act) %in% paste0("MIN",1:1440))
        if(!identical(act[,-act_cols], flags[,-act_cols])){
            stop("One or more columns of the act and flags do not match. Please double check that these two dataframes are identical except for the activity count and wear/non-wear columns")
        }
        flags = flags[,act_cols]
        if(!all(as.vector(as.matrix(flags))) %in% c(0,1,NA)){
                stop("Wear/non-wear flags need to be either 0 (non-wear), 1 (wear), or NA (missing)")
        }
        flag_nonwear <- rowSums(flags[, act_cols], na.rm = TRUE) < threshold_lower
        rm(flags)


        stopifnot(all(is.finite(act$PAXSTAT),is.finite(act$PAXCAL)))

        cond <- c("flag_nonwear", "!(act$PAXSTAT %in% 1)","!(act$PAXCAL %in% 1)")[c(TRUE, rm_PAXSTAT, rm_PAXCAL)]
        cond <- paste(cond, collapse="|")

        eval(parse(text=paste("return(which(!(",cond,")))")))
}




#' Reweight NHANES accelerometry data
#'
#' @description
#' This function re-weights accelerometry data for NHANES 2003-2004,2005-2006 waves.
#'
#' @param data Data frame to with survey weights to be re-weighted. Should not contain any duplicated participants. That is, each row
#' of this dataframe should correspond to a unique value of SEQN.
#' The data frame supplied to data must have the columns: SEQN", SDDSRVYR,WTMEC2YR, and WTINT2YR.
#'
#' @param return_unadjusted_wts Logical value indicating whether to return the unadjusted 2-year and, if applicable, 4-year survey weights for all participants.
#'
#' @param age_bks Vector of ages which define the intervals used for re-weighting. This argument is passed to the \code{\link{cut}} function to create age categories which are in turn
#' used to re-weight participants. The argument "right" determines whether these intervals will be closed on the right or the left.
#'
#' @param right Logical value indicating whether the age intervals defined by the "age_bks" arguement should be closed on the left (right=FALSE) or the right (right=TRUE).
#' See \code{\link{cut}} for additional details and examples. Defaults to TRUE.
#'
#' @details
#'
#' The reweight_accel function is designed to re-weight only the 2003-2004 and 2005-2006 waves in the context of missing data.
#' This function calculates 2- and 4- year adjusted and unadjusted survey weights.
#' The re-weighting is performed using age, sex, and ehtnicity strata applied to each wave separately.
#' More specifically, individuals in the data frame supplied to the function via the
#' "data" argument are upweighted by a factor such that the sum of their weights is equal to the total survey weight in the
#' population strata. If data are missing completely at random within each of these strata, then these re-weighted strata are
#' representative of the corresponding strata in the larger study.
#'
#' Users should ensure that if they intend to use the adjusted weights calculated by this function, that the data they reweight aligns with
#' the re-weighted strategy, particularly with regard to age. That is, it does make sense to reweight all individuals 58-60 to be representative of
#' all individuals ages 50-60. The age categories used in re-weighting are controlled by the "age_bks" argument. In illustrate the problems of misalignment of ages
#' in the examples below. Moreover, the re-weighting is done separately for the interview and examination weights. Because there is a time lag between the interview and the exam, individuals
#' may belong to different age strata for the purposes of re-weighting the interview and examination survey weights. Therefore, users need to make sure the
#' ages in their data align with the survey weight they intend to use.
#'
#' It is possible that if there are one or more strata that are sparse, the survey weights. Users should always inspect the adjusted survey weights
#' for outliers.
#'
#'
#'
#'
#'
#' @return
#'
#' The function reweight_accel will return a dataframe with the same columns as the data frame supplied to the "data" argument with either 8 or 16 additional columns.
#' If the data supplied to the reweight_accel function only comes from one NHANES wave, then only the 2-year survey weights will be returned.
#' If there are data from both the 2003-2004 and 2005-2006 waves supplied to the reweight_accel function, then both the 2-year and 4-year survey weights will be
#' returned. Any time an analysis is done using the combined data, the appropriate 4-year survey weight should be used.
#'
#' These survey weights are described below.
#'
#'
#'
#' \itemize{
#'      \item{Examination survey weights}
#'      \itemize{
#'           \item{wtmec2yr_adj: }{The age, gender, and ethnicity re-weighted 2-year survey weight}
#'           \item{wtmec2yr_adj_norm: }{Normalized version of wtmec2yr_adj. This is calculated as wtmec2yr_adj/mean(wtmec2yr_adj)}
#'           \item{wtmec4yr_adj: }{The age, gender, and the ethnicity re-weighted 4-year survey weight. This is calculated as wtmec2yr_adj/2.}
#'           \item{wtmec4yr_adj_norm: }{Normalized version of wtmec4yr_adj. This is calculated as wtmec4yr_adj/mean(wtmec4yr_adj)}
#'           \item{wtmec2yr_unadj: }{Unadjusted 2-year examination weight. This is just a copy of the WTMEC2YR variable.}
#'           \item{wtmec2yr_unadj_norm: }{Normalized version of wtmec2yr_adj. This is calculated as wtmec2yr_unadj/mean(wtmec2yr_unadj)}
#'           \item{wtmec4yr_unadj: }{Unadjusted 4-year examination weight. This is calculated as wtmec2yr_unadj/2.}
#'           \item{wtmec4yr_unadj_norm: }{Normalized version of wtmec4yr_unadj. This is calculated as wtmec4yr_unadj/mean(wtmec4yr_unadj)}
#'      }
#'
#'      \item{Interview survey weights}
#'      \itemize{
#'           \item{wtint2yr_adj: }{The age, gender, and ethnicity re-weighted 2-year survey weight}
#'           \item{wtint2yr_adj_norm: }{Normalized version of wtint2yr_adj. This is calculated as wtint2yr_adj/mean(wtint2yr_adj)}
#'           \item{wtint4yr_adj: }{The age, gender, and the ethnicity re-weighted 4-year survey weight. This is calculated as wtint2yr_adj/2.}
#'           \item{wtint4yr_adj_norm: }{Normalized version of wtint4yr_adj. This is calculated as wtint4yr_adj/mean(wtint4yr_adj)}
#'           \item{wtint2yr_unadj: }{Unadjusted 2-year examination weight. This is just a copy of the wtint2YR variable.}
#'           \item{wtint2yr_unadj_norm: }{Normalized version of wtint2yr_adj. This is calculated as wtint2yr_unadj/mean(wtint2yr_unadj)}
#'           \item{wtint4yr_unadj: }{Unadjusted 4-year examination weight. This is calculated as wtint2yr_unadj/2.}
#'           \item{wtint4yr_unadj_norm: }{Normalized version of wtint4yr_unadj. This is calculated as wtint4yr_unadj/mean(wtint4yr_unadj)}
#'      }
#'
#'
#' }
#'
#' If any of the 14 columns described above are already in the dataframe supplied to the data argument, they will be overwritten and a
#' warning will be printed to the console. This may occur when an individual subsets their data multiple times and re-weights at each step.
#'
#' @examples
#'
#'
#' \dontrun{
#' library("rnhanesdata")
#' set.seed(1241)
#' ## load the 2003-2004 demographic data
#' data("Covariate_C")
#'
#' ## consider just those individuals between the ages in the interval [50,80)
#' ## at the exam portion of the study
#' df50 <- subset(Covariate_C, RIDAGEEX/12 >= 50 & RIDAGEEX/12 <80)
#'
#' ## subsample 75% of these individuals, then re-weight the data
#' df50_sub <- df50[sample(1:nrow(df50), replace=FALSE, size=floor(nrow(df50)*0.75)),]
#' df50_rw  <- reweight_accel(df50)
#'
#' ## check the unadjusted weights 2-year weights match the WTMEC2YR variable
#' sum(df50_rw$WTMEC2YR != df50_rw$wtmec2yr_unadj)
#'
#' ## See that the adjusted interview weights are massively inflated
#' ## This is because there are individuals who are in the [40,50) strata during the interview
#' ## by are in the [50,60) strata for the exam. These few individuals are upweighted to
#' ## "represent" all individuals [50,60) during the interview, which clearly doesn't make sense.
#' summary(df50_rw$wtint2yr_adj)
#'
#' ## Subsetting the reweighted dataset
#'
#'
#'
#'
#' }
#'
#' @export
reweight_accel <- function(data, return_unadjusted_wts=TRUE,
                           age_bks = c(0, 1, 3, 6, 12, 16, 20, 30, 40, 50, 60, 70, 80, 85, Inf),
                           right=FALSE){
        stopifnot(all(c("SEQN","SDDSRVYR","WTMEC2YR","WTINT2YR") %in% colnames(data)))
        stopifnot(all(data$SDDSRVYR %in% c(3,4)))
        if(any(duplicated(data$SEQN))) stop("Data must be in the form of one row per participant")

        ret <- data

        vars_wts <- c("wtint2yr_unadj", "wtmec2yr_unadj",
                      "wtint2yr_unadj_norm","wtmec2yr_unadj_norm",
                      "wtint4yr_unadj", "wtint4yr_unadj_norm",
                      "wtmec4yr_unadj", "wtmec4yr_unadj_norm",
                      "wtint2yr_adj", "wtint2yr_adj_norm",
                      "wtint4yr_adj", "wtint4yr_adj_norm",
                      "wtmec4yr_adj", "wtmec4yr_adj_norm")

        if(any(vars_wts %in% colnames(data))){
            warning(paste0("Variables:",  paste0(vars_wts[vars_wts %in% colnames(data)],collapse=", ") ," found in data. These have been overwritten."))
        }

        for(i in vars_wts) ret[[i]] <- NULL
        rm(list=c("vars_wts","i"))


        uwave     <- sort(unique(ret$SDDSRVYR))
        n_age_bks <- length(age_bks)


        if(return_unadjusted_wts){
            ret$wtint2yr_unadj <- ret$WTINT2YR
            ret$wtmec2yr_unadj <- ret$WTMEC2YR

            ret$wtint2yr_unadj_norm <- ret$WTINT2YR/mean(ret$WTINT2YR)
            ret$wtmec2yr_unadj_norm <- ret$WTMEC2YR/mean(ret$WTMEC2YR)

            ## calculate raw/normalized unadjusted 4-year weights
            if(length(uwave) > 1){
                ret$wtint4yr_unadj      <- ret$WTINT2YR/2
                ret$wtint4yr_unadj_norm <- ret$wtint4yr_unadj/mean(ret$wtint4yr_unadj)

                ret$wtmec4yr_unadj      <- ret$WTMEC2YR/2
                ret$wtmec4yr_unadj_norm <- ret$wtmec4yr_unadj/mean(ret$wtmec4yr_unadj)
            }
        }



        data(list=c("Covariate_C","Covariate_D"), envir=environment(), package="rnhanesdata")
        demo <- rbind(Covariate_C, Covariate_D)


        ## create age categories, 85+ are coded as missing so impute a value >= 85
        demo$age_mn                     <- demo$RIDAGEMN/12
        demo$age_mn[is.na(demo$age_mn)] <- 86

        demo$age_ex                     <- demo$RIDAGEEX/12
        demo$age_ex[is.na(demo$age_ex)] <- 86

        demo$age_cat_mn <- cut(demo$age_mn, breaks=age_bks, right=right)
        demo$age_cat_ex <- cut(demo$age_ex, breaks=age_bks, right=right)

        demo$Race2 <- factor(demo$Race, levels=c("Mexican American", "Other Hispanic","White","Black","Other"),
                             labels=c("Mexican American", "Other", "Other","Black","Other"))


        wtmec2yr_adj <- wtint2yr_adj <- rep(NA, nrow(ret))
        for(i in seq_along(uwave)){
            for(j in levels(demo$Gender)){
                for(k in levels(demo$Race2)){
                    for(l in levels(demo$age_cat_mn)){
                        inx_int_full <- which(demo$Gender == j & demo$Race2 == k & demo$age_cat_mn == l & demo$SDDSRVYR==uwave[i])
                        inx_mec_full <- which(demo$Gender == j & demo$Race2 == k & demo$age_cat_ex == l & demo$SDDSRVYR==uwave[i])
                        seqn_int     <- demo$SEQN[which(demo$Gender == j & demo$Race2 == k & demo$age_cat_mn == l & demo$SDDSRVYR==uwave[i])]
                        seqn_mec     <- demo$SEQN[which(demo$Gender == j & demo$Race2 == k & demo$age_cat_ex == l & demo$SDDSRVYR==uwave[i])]

                        inx_ret_int <- which(ret$SEQN %in% seqn_int)
                        inx_ret_mec <- which(ret$SEQN %in% seqn_mec)


                        if(length(inx_ret_int) > 0){
                            wt_int_full <- sum(demo$WTINT2YR[inx_int_full])
                            wt_int_ret  <- sum(ret$WTINT2YR[inx_ret_int])

                            wtint2yr_adj[inx_ret_int] <- ret$WTINT2YR[inx_ret_int]*wt_int_full/wt_int_ret
                        }

                        if(length(inx_ret_mec) > 0){
                            wt_mec_full <- sum(demo$WTMEC2YR[inx_mec_full])
                            wt_mec_ret  <- sum(ret$WTMEC2YR[inx_ret_mec])

                            wtmec2yr_adj[inx_ret_mec] <- ret$WTMEC2YR[inx_ret_mec]*wt_mec_full/wt_mec_ret
                        }
                    }
                }
            }

        }



        ret$wtint2yr_adj      <- wtint2yr_adj
        ret$wtint2yr_adj_norm <- ret$wtint2yr_adj/mean(ret$wtint2yr_adj)

        ret$wtmec2yr_adj      <- wtmec2yr_adj
        ret$wtmec2yr_adj_norm <- ret$wtmec2yr_adj/mean(ret$wtmec2yr_adj)

        if(length(uwave) > 1){
            ret$wtint4yr_adj      <- ret$wtint2yr_adj/2
            ret$wtint4yr_adj_norm <- ret$wtint4yr_adj/mean(ret$wtint4yr_adj)

            ret$wtmec4yr_adj      <- ret$wtmec2yr_adj/2
            ret$wtmec4yr_adj_norm <- ret$wtmec4yr_adj/mean(ret$wtmec4yr_adj)
        }

        ret
}




