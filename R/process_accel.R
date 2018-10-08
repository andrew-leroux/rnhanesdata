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
#' @param local logical argument indicating whether the zippped raw .xpt accelerometry files are stored locally.
#' If FALSE, will download the data into a temporary file from the CDC website and process the data. If TRUE,
#' the zipped data will be sourced locally. Defaults to FALSE.
#'
#' @param localpath character string indicating where the locally zipped raw .xpt files are.
#'
#' @param urls character
#'
#' @param write logical argument indicating whether a .rda file should be created for each wave of processed data.
#'   Defaults to FALSE. If TRUE, each wave of data will be saved in the location specified by the localpath variable. Each
#'   wave will be saved as an .rda file named PAXINTEN_\*.rda where \* corresponds to the letter asspciated with a particular NHANES wave.
#'
#' @param deleteraw logical scalar indicating whether to delete the (un)zipped .xpt files after reading them into R. If deleteraw=TRUE and zipped=TRUE,
#' both the .xpt and .ZIP files are deleted locally. Defaults to FALSE.
#'
#' @param zipped logical scalar indicating whether the physical activity files are in the zipped format downloaded directly from the CDC website (.ZIP).
#' If local=FALSE and the data are downloaded from the CDC's website, this argument is ignored.
#'
#' @param compress Character scalar indicating the type of compression to use when write=TRUE. The default, "xz" results in small file sizes,
#' but is not compatable with some old versions of R. Must be one of: "xz", "gzip", or "bzip2". See \code{\link{save}} for more details.
#'
#'
#' @details
#'
#' The function process_accel can be used to process the NHANES 2003-2004 and 2005-2006 waves accelerometry data.
#'
#' @return
#'
#' This function will return a list with number of elements less than or equal to the number of waves of data specified by the names_accel_xpt
#' argument. The exact number of elements returned will depend on whether all files specified by the user are found in either: 1) the local directory
#' indicated by the localpath argument; or 2) downloadable from the website(s) indicated by the "urls" argument. Each element of the list returned is a data frame
#' with columns:
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
#'    \item{MIN1-MIN1440:}{ Activity count corresponding to each minute of the day. For example, MIN1 is the activity count for 00:00-00:01. }
#' }
#'
#' Although process_accel will try to process any ".xpt" or ".ZIP" file which follows the NHANES accelerometry naming convention, it has only been tested
#' on the NHANES 2003-2006 waves' accelerometry data. As future NHANES accelerometry data are released, we intend to verify that process_accel
#' will correctly transform the newly released data into our 1440+ format.
#' The function documentation, and, if necessary the function itself, will be updated as needed going forward.
#'
#' @examples
#' \dontrun{
#' library("rnhanesdata")
#' ## download and
#' process_accel()
#' process_flags()
#' }
#'
#' @importFrom haven read_xpt
#'
#' @importFrom utils write.csv unzip
#'
#' @export
process_accel <- function(names_accel_xpt = c("PAXRAW_C","PAXRAW_D"),
                          write=FALSE, local=FALSE, localpath=NULL, urls=NULL,
                          deleteraw=FALSE, zipped=TRUE, compress="xz"){

        if(local & is.null(localpath)){
                stop("If specifying local = TRUE, must specify localpath as a local directory where the zipped data files are saved.")
        }
        if(!dir.exists(localpath)){
                stop("localpath must be a valid local directory path.")
        }
        if(any(!grepl("^PAXRAW_[A-Z]$", names_accel_xpt))){
                stop("Raw accelerometer data must follow the NHANES naming convention of PAXRAW_* where * denotes the letter corresponding the NHANES wave")
        }
        if(any(!local & is.null(urls), !local & length(urls) != length(names_accel_xpt)) ){
                stop("If the data is not locally stored urls must be a vector of the same length as names_accel_xpt.")
        }
        if(any(!paste0(names_accel_xpt,".ZIP") %in% list.files(localpath)) & local & zipped){
                stop(paste0("One or more of ", paste0(names_accel_xpt,".ZIP"), " not found in localpath directory."))
        }
        if(any(!paste0(names_accel_xpt,".xpt") %in% list.files(localpath)) & local & !zipped){
            stop(paste0("One or more of ", paste0(names_accel_xpt,".xpt"), " not found in localpath directory."))
        }
        stopifnot(compress %in% c("xz","gzip","bzip2"))

        out.name <- gsub("PAXRAW", "PAXINTEN", names_accel_xpt)
        ret <- c()
        for(i in seq_along(names_accel_xpt)){
                datapath <- c()
                # if local=FALSE, download the data directly from the link provided in URLS
                if(!local){
                        datapath <- system.file("extdat",package="nhanesdata")
                        temp <- tempfile()
                        download.file(urls[i], temp)
                        sim.data <- read_xpt(unzip(temp,
                                                   tolower(paste0(names_accel_xpt[i],".xpt"))))
                        unlink(temp)
                }
                if(local){
                        datapath <- localpath
                        if(zipped){
                            sim.data <- read_xpt(unzip(paste0(datapath, names_accel_xpt[i],".ZIP"),
                                                       tolower(paste0(names_accel_xpt[i],".xpt")),
                                                       exdir=datapath)
                                                )
                        }
                }
                if(deleteraw){
                    ext_tmp <- c(".xpt")
                    if(zipped) ext_tmp <- c(ext_tmp, ".ZIP")

                    file.remove(paste0(datapath, names_accel_xpt[i],ext_tmp))
                    rm(list=c("ext_tmp"))
                }


                uid <- as.character(unique(sim.data$SEQN))

                ## read_xpt reads all variables in as numeric, check for truncation on reading in
                stopifnot(all(nchar(uid)==5))

                ## create empty data frame with full 7 days of data for each subject (10,080 rows/subject)
                n    <- length(uid)
                seqn <- rep(uid,each=10080)
                paxn <- rep(c(1:10080),n)
                full.list <- data.frame(SEQN=seqn,PAXN=paxn)
                rm(list=c("n","seqn","paxn"))

                ## merge data sets to create data will include NAs for missing days/times
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

                if(write){
                        eval(parse(text=paste0("assign(\"",out.name[i], "\", value=ret[[out.name[i]]])")))
                        eval(parse(text=paste0("save(", out.name[i],",file=\"", datapath, out.name[i], ".rda\", compress=", compress,")")))
                        message(paste0("Wave ", i, " Saved as: ", datapath, out.name[i], ".rda"))
                        rm(list=c(out.name[i]))
                }

                rm(list=c("pax","pax.wide","col.name","idweekday"))

                message(paste(names_accel_xpt[i], "Processed"))
        }
        ret
}
















#' Process wear/non-wear flags for NHANES 2003-2004 and 2005-2006 accelerometry data
#'
#' @description
#' This function creates wear/non-wear flag matrices for processed NHANES 2003-2004 and 2005-2006 accelerometry data
#'
#'
#'
#' @param x A list with each element corresponding to a data matrix of activity counts in the 1440+ format with 7 rows per individual.
#' The data should be sorted by participant (SEQN) and then in descending order chronologically. The output of \code{\link{process_accel}} can be fed directly to
#' this argument. See examples.
#'
#' @param write Logical value indicating whether a .rda file should be created for each wave of processed data.
#'   Defaults to FALSE. If TRUE, each wave of data will be saved in the location specified by the localpath variable. Each
#'   wave will be saved as an .rda file named Flags_\*.rda where \* corresponds to the letter asspciated with a particular NHANES wave.
#'
#'
#' @param localpath character string indicating where the locally processed .rda files should be saved if write = TRUE.
#'
#' @param window size of the moving window used to assess non-wear in minutes. Defaults to 90 minutes.
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
#' @param compress Character scalar indicating the type of compression to use when write=TRUE. The default, "xz" results in small file sizes,
#' but is not compatable with some old versions of R. Must be one of: "xz", "gzip", or "bzip2". See \code{\link{save}} for more details.
#'
#' @param ... aditional arguments to be passed to \code{\link{weartime}}.
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
#' process_accel(write=FALSE)
#' }
#'
#' @references
#'
#' @importFrom accelerometry weartime
#'
#' @importFrom utils write.csv
#'
#' @export
process_flags <- function(x,
                          write=FALSE, localpath=NULL, days_distinct=FALSE,
                          window=90L, tol=2L, tol_upper=99L,
                          compress="xz", ...){
        names_accel_xpt <- names(x)
        if(any(!grepl("^PAXINTEN_[A-Z]$", names_accel_xpt)) | is.null(names_accel_xpt)){
                stop("x must be a list with each element corresponding to PAXINTEN_* where * denotes the letter corresponding the NHANES wave")
        }
        stopifnot(compress %in% c("xz","gzip","bzip2"))
        out.name <- paste0("Flags_", substr(names_accel_xpt, nchar(names_accel_xpt), nchar(names_accel_xpt)))

        ret <- c()

        for(i in seq_along(names_accel_xpt)){
                full_data <- x[[i]]

                if(any(table(full_data$SEQN) != 7)){
                        stop(paste0("Activity data ", names_accel_xpt[i], " does not have 7 rows of data per subject. Please double check Activitiy count data."))
                }

                activity_data <- as.matrix(full_data[,paste0("MIN",1:1440)])
                activity_data[is.na(activity_data)] = 0    # replace NAs with zeros

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

                if(write){
                        if(is.null(localpath)){
                                localpath <- paste0(getwd(), .Platform$file.sep, out.name[i])
                        }
                        eval(parse(text=paste0(out.name[i], "<- out")))
                        eval(parse(text=paste0("save(", out.name[i], ", file=file.path(localpath, paste0(out.name[i], \".rda\", compress=",compress,")))")))
                }
                rm(list=c("out","full_data","WMX","uid","activity_data"))

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
#' @param write logical argument indicating whether a .rda file of wear/non-wear flags
#' should be created for each wave of processed data. Defaults to FALSE.
#'
#' @param localpath Character scalar describing the location where . If NULL, the funciton will look in pacakge data directory for the requested raw mortality data.
#' Defaults to NULL.
#'
#'
#' @details
#'
#' As of writing, this funciton has only been tested on the 2011 release for the 2003-2004 and 2005-2006 NHANES mortality data.
#' The raw data comes in the form of a vector of strings, with each string associated with on participant.
#' The location of relevant variables withn each string is described in the document .... (see references)
#'
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
#'          \describe{
#'                  \item{1}{ Eligible}
#'                  \item{2}{ Under age 18, not available for public release}
#'                  \item{3}{ Ineligible}
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
#' \dontrun{
#'
#'
#' }
#'
#' @references
#'
#' NHANES linked public mortality files are described at the following link: \url{https://www.cdc.gov/nchs/data-linkage/mortality-public.htm}.
#'
#'
#'
#'
#' @export
process_mort <- function(waves=c("C","D"),
                         mort_release_yr=2011,
                         write=FALSE,
                         localpath=NULL){

        if(!all(waves %in% LETTERS)) stop("One or more waves invalid. waves must be identified as a captial letter A-Z.")
        if(!is.numeric(mort_release_yr)) stop("mort_release_yr must be numeric")


        waves <- sort(waves)

        waves_num  <- vapply(waves, function(x) seq(1999, 2049, by=2)[which(LETTERS == x)], numeric(1))
        waves_mort <- paste0("NHANES_",waves_num,"_",waves_num+1,"_MORT_",mort_release_yr,"_PUBLIC.dat")

        if(is.null(localpath)) {
                filepath <- file.path(system.file("extdat","mort",package="nhanesdata"), waves_mort)
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



                if(write){
                        if(is.null(localpath)){
                                writepath <- paste0(getwd(), .Platform$file.sep)
                        }
                        eval(parse(text=paste0(out.name, "<- out")))
                        eval(parse(text=paste0("save(", out.name, ", file=file.path(writepath, paste0(out.name, \".rda\")))")))
                        rm(list=c("writepath"))
                }
                rm(list=c("out","out.name","seqn","eligstat","mortstat","causeavl","permth_exm","permth_int",
                          "ucod_leading","diabetes","hyperten","j","raw.data",
                          paste0("mortsrce_",c("ndi","cms","ssa","dc","dcl")))
                   )

        }

        ret


}








#' Merge non-accelerometry data for NHANES waves
#'
#' @description
#' This function merges covariate data
#' from one or more  NHANES data files across one or more waves of the study.
#' This function will only recognize those files which have "SEQN" as their first column name as this is the format across all NHANES data files.
#'
#'
#' @param waves character vector with entries of (capitalized) letter of the alphabet corresponding to the
#' NHANES wave of interest. Defaults to a vector containing "C" and "D" corresponding to the NHANES 2003-2004 and 2005-2006 waves.
#'
#' @param varnames character vector indicating which column names are to be searched for.
#' Will check all .XPT files in located in the directory specified by dataPath. If extractAll = TRUE, then this argument is effectively ignored.
#'
#' @param localpath file path where covariate data are saved. Covariate data must be in .XPT format,
#' and should be in their own folder. For example, PAXRAW_C.XPT should not be located in the folder with
#' your covariate files. This will not cause an error, but the code will take much longer to run.
#'
#' @param write logical argument indicating whether a .rda file of covariate data. Defaults to FALSE.
#'
#' @param extractAll logical argument indicating whether all columns of all .XPT files in the search path should be returned.
#' If extractALL = TRUE, all variables from all .XPT files with
#' Defaults to FALSE.
#'
#'
#' @return
#'
#' This function will return a list with number of elements less than or equal to the number of waves of data specified by the "waves"
#' argument. The exact number of elements returned will depend on whether all files specified by the user are found in either: 1) the local directory
#' indicated by the localpath argument; or available in the data package. Each element of the list returned is a data frame. The columns of the data frame
#' will depend on the variables specified in the "varnames" argument and whether those variable are available for some (or any) of the waves specfied by the waves argument.
#' At a minimum, the returned dataframe will contain a column for each subject identifier (SEQN).
#'
#' If the demographic datasets are not in the directory specified by localpath, there is no guarantee that all participants in a wave will be included in the retunred results.
#'
#'
#'
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#'
#' @references
#'
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
                          write=FALSE,
                          extractAll=FALSE){

        if(!all(waves %in% LETTERS)) stop("One or more waves invalid. waves must be identified as a captial letter A-Z.")
        stopifnot(length(waves) >= 1)
        stopifnot(is.vector(waves))

        waves <- sort(waves)
        ## ensure ID variable is in search
        varnames <- unique(c('SEQN',varnames))
        ## find all files of .xpt structure that correspond to the year specified in the data
        if(is.null(localpath)){
                localpath <- system.file("extdat/covar/",package="nhanesdata")
        }
        files_full   <- list.files(localpath)

        ret <-c()

        for(i in seq_along(waves)){
                cohort <- waves[i]

                pathExt <- paste0('_', cohort, '.XPT', sep='')
                ## find only those files associated with a particular NHANES wave
                files   <- files_full[substr(files_full, (nchar(files_full) - 5), nchar(files_full)) == pathExt]

                if(length(files) == 0) next

                ## find which files contain the variables requested by the user
                covarMats <- lapply(files, function(x){
                        mat <- try(read_xpt(paste0(localpath,x)))
                        if(inherits(mat, "try-error")) return(NULL)
                        if(!"SEQN" %in% colnames(mat)) return(NULL)
                        ## expecting SEQN to be first column in all NHANES datasets
                        if(!"SEQN" == colnames(mat)[1]) return(NULL)
                        ## if extractAll = FALSE, return only those columns specified in the varnames argument
                        if(!extractAll){
                                if(length(setdiff(intersect(colnames(mat),varnames), "SEQN"))==0) return(NULL)
                                mat <- mat[,colnames(mat)%in%varnames,drop=FALSE]
                        }
                        ## if extractAll = TRUE, return all columns
                        if(!is.null(dim(mat))) {
                                mat
                        }
                        else NULL
                })
                ##
                covarMats <- covarMats[!vapply(covarMats, is.null,logical(1))]
                matchedNames <- lapply(covarMats, colnames)
                numMatched   <- length(unlist(matchedNames))

                if(numMatched == 0) stop('Error: No Variable Names Recognized for this Year/Variable Combination')
                if(numMatched > 0 & !extractAll)  message(
                        paste("For", cohort, "cohort,",
                              (numMatched - length(matchedNames)),
                              'Covariates Found of', (length(varnames)-1),'specified.',
                              'Missing covariates:',
                              paste(setdiff(varnames, unlist(matchedNames)),collapse=", ") ))

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
                                paste("Variables with repeated observations per subject found for the following variables:",
                                      paste(sapply(covarMats[rep_inx], function(x) colnames(x)[-1])  , collapse=","),
                                      "Note that these variables will be stored with class AsIs() objects in resulting data frames. ",
                                      "See ?I for details on AsIs class.")
                                )
                }


                out.name <- paste0("Covariate_", cohort)

                if(write){
                        if(is.null(localpath)){
                                writepath <- paste0(getwd(), .Platform$file.sep, out.name[i])
                        }
                        eval(parse(text=paste0(out.name, "<- CovarMat")))
                        eval(parse(text=paste0("save(", out.name, ", file=file.path(writepath, paste0(out.name[i], \".rda\")))")))
                }

                ret[[out.name]] <- CovarMat

                rm(list=c("CovarMat","out.name"))
        }

        ret

}



#' Remove days with too few/too much weartime and NHANES data quality flags.
#'
#' @description
#' This function subsets accelerometry data by wear/non-wear time criteria, as well as
#' NHANES data quality flags.
#'
#' @param act Activity data matrix
#'
#' @param flags Wear/Non-wear flag matrix
#'
#' @param threshold upper limit for number of non-wear minutes. Defaults to 600 minutes (10 hours).
#'
#' @param rm_PAXSTAT Logical argument indicating whether to remove based on the data reliability indicator variable PAXSTAT. Defaults to TRUE.
#'
#' @param rm_PAXCAL Logical argument indicating whether to remove based on the accelerometer calibration indicator variable PAXCAL. Defaults to TRUE.
#'
#' @param return_act Not currently implemented.
#'
#'
#' @examples
#' \dontrun{
#' process_accel(write=FALSE)
#' }
#'
#' @references
#'
#'
#' @export
exclude_accel <- function(act, flags, threshold_lower = 600, rm_PAXSTAT = TRUE, rm_PAXCAL = TRUE){
        stopifnot(all(is.data.frame(act), is.data.frame(flags)))
        stopifnot(all(colnames(act) == colnames(flags)))
        stopifnot(all(c("PAXSTAT", "PAXCAL", paste0("MIN",1:1440)) %in% colnames(act)))

        stopifnot(all(is.finite(act$PAXSTAT),is.finite(act$PAXCAL)))
        flag_nonwear <- rowSums(flags[, paste('MIN', 1:1440, sep='')], na.rm = TRUE) < threshold_lower

        ## remove nonwear days and days flagged by NHANES
        cond <- c("flag_nonwear", "act$PAXSTAT!=1","act$PAXCAL!=1")[c(TRUE, rm_PAXSTAT, rm_PAXCAL)]
        cond <- paste(cond, collapse="|")

        if(return_act) return(eval(parse(text=paste("return(act[!(",cond, "),])"))))

        eval(parse(text=paste("return(which(!(",cond,")))")))
}




#' Reweight NHANES accelerometry data
#'
#' @description
#' This function re-weights accelerometry data for NHANES 2003-2004,2005-2006 waves.
#'
#' @param data data frame to with survey weights to be re-weighted.Should only contain one subject per ror.
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
#' The reweight_accel function is designed to re-weight only the 2003-2004 and 2005-2006 waves.
#'
#'
#' @return
#'
#' The function reweight_accel will return a dataframe with the same columns as the data frame supplied to the "data" arguement
#'
#' @examples
#' \dontrun{
#' process_accel(write=FALSE)
#' }
#'
#' @references
#'
#'
#' @export
reweight_accel <- function(data, return_unadjusted_wts=TRUE,
                           age_bks = c(0, 1, 3, 6, 12, 16, 20, 30, 40, 50, 60, 70, 80, 85, Inf),
                           right=FALSE){
        stopifnot(all(c("SEQN","SDDSRVYR","WTMEC2YR","WTINT2YR") %in% colnames(data)))
        stopifnot(all(data$SDDSRVYR %in% c(3,4)))

        ret <- data

        ret$wtint2yr_unadj_norm <- ret$wtmec2yr_unadj_norm <-
            ret$wtint4yr_unadj <- ret$wtint4yr_unadj_norm <-
            ret$wtmec4yr_unadj <- ret$wtmec4yr_unadj_norm <-
            ret$wtint2yr_adj <- ret$wtint2yr_adj_norm <-
            ret$wtmec2yr_adj <- ret$wtmec2yr_adj_norm <-
            ret$wtint4yr_adj <- ret$wtint4yr_adj_norm <-
            ret$wtmec4yr_adj <- ret$wtmec4yr_adj_norm <- NULL




        uwave     <- sort(unique(ret$SDDSRVYR))
        n_age_bks <- length(age_bks)


        if(return_unadjusted_wts){
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




