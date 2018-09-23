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
#' @param write logical argument indicating whether a .rda file should be created for each wave of processed data.
#'   Defaults to FALSE. If TRUE, each wave of data will be saved in the location specified by the localpath variable. Each
#'   wave will be saved as an .rda file named PAXINTEN_\*.rda where \* corresponds to the letter asspciated with a particular NHANES wave.
#'
#' @param deleteraw logical argument indicating whether to delete the unzipped .xpt files after reading them into R.
#'
#' @examples
#' \dontrun{
#' library("nhanesdata")
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
                          write=FALSE, local=FALSE,
                          localpath=NULL, deleteraw=TRUE,
                          urls=NULL){

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
        if(any(!paste0(names_accel_xpt,".ZIP") %in% list.files(localpath)) & local){
                stop(paste0("One or more of ", paste0(names_accel_xpt,".ZIP"), " not found in localpath directory."))
        }

        out.name <- gsub("PAXRAW", "PAXINTEN", names_accel_xpt)
        ret <- c()
        for(i in seq_along(names_accel_xpt)){
                datapath <- c()
                # if local=FALSE, download the data directly from the link provided in URLS
                if(!local){
                        datapath <- system.file("extdat","act",package="nhanesdata")
                        temp <- tempfile()
                        download.file(urls[i], temp)
                        sim.data <- read_xpt(unzip(temp,
                                                   tolower(paste0(names_accel_xpt[i],".xpt"))))
                        unlink(temp)
                        if(deleteraw) file.remove(paste0(datapath, names_accel_xpt[i],".xpt"))
                }
                if(local){
                        datapath <- localpath
                        sim.data <- read_xpt(unzip(paste0(datapath, names_accel_xpt[i],".ZIP"),
                                                   tolower(paste0(names_accel_xpt[i],".xpt")),
                                                   exdir=datapath)
                        )
                        if(deleteraw) file.remove(paste0(datapath, names_accel_xpt[i],".xpt"))
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
                        eval(parse(text=paste0("save(", out.name[i],",file=\"", datapath, out.name[i], ".rda\")")))
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
#' @param write logical argument indicating whether a .rda file should be created for each wave of processed data.
#'   Defaults to FALSE. If TRUE, each wave of data will be saved in the location specified by the localpath variable. Each
#'   wave will be saved as an .rda file named Flags_\*.rda where \* corresponds to the letter asspciated with a particular NHANES wave.
#'
#'
#' @param localpath character string indicating where the locally processed .rda files should be saved if write = TRUE.
#'
#' @param window size of the moving window used to assess non-wear in minutes. Defaults to 90 minutes.
#' See \code{\link{weartime}} for more details.
#'
#' @param tol maximum number of minutes with counts greater than 0 within the window allowed before a particular
#' minute is considered "wear". That is, if for a given minute, the window around that minute has tol + 1
#' activity counts greater than 0, this miute is considered "wear".
#' See \code{\link{weartime}} for more details.
#'
#' @param tol_upper maximium activity count for any minute within the window
#' Defaults to 99. That is, for a given minute, if the window contains any minutes with
#' activity counts greater than tol.upper, this minute is considered "wear".
#' See \code{\link{weartime}} for more details.
#'
#' @param ... aditional arguments to be passed to \code{\link{weartime}}.
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
                          window=90L, tol=2L, tol_upper=99L, ...){
        names_accel_xpt <- names(x)
        if(any(!grepl("^PAXINTEN_[A-Z]$", names_accel_xpt))){
                stop("x must be a list with each element corresponding to PAXINTEN_* where * denotes the letter corresponding the NHANES wave")
        }
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
                                writepath <- paste0(getwd(), .Platform$file.sep, out.name[i])
                        }
                        eval(parse(text=paste0(out.name[i], "<- out")))
                        eval(parse(text=paste0("save(", out.name[i], ", file=file.path(writepath, paste0(out.name[i], \".rda\")))")))
                }
                rm(list=c("out","full_data","WMX","uid","activity_data"))

        }
        class(ret) <- c(class(ret), "flags1440")
        ret

}










#' Process mortality data for NHANES 2003-2004 and 2005-2006 waves
#'
#' @description
#' This function creates a clean mortality dataset which can be combined with data from the
#' NHANES 2003-2004/2005-2006 waves.
#'
#'
#'
#' @param write logical argument indicating whether a .rda file of wear/non-wear flags
#' should be created for each wave of processed data. Defaults to FALSE.
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
#' \url{https://www.cdc.gov/nchs/data-linkage/mortality-public.htm}
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

                out.name <- paste0("Mortality_", waves[i])
                out <- data.frame("SEQN"=as.integer(seqn),
                                  "eligstat"=as.integer(eligstat),
                                  "mortstat"=as.integer(mortstat),
                                  "causeavl"=as.integer(causeavl),
                                  "permth_exm"=permth_exm,
                                  "permth_int"=permth_int,
                                  "ucod_leading"=ucod_leading,
                                  "diabetes_cause_mort"=as.integer(diabetes),
                                  "hyperten_cause_mort"=as.integer(hyperten),
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
#' @param dataPath file path where covariate data are saved. Covariate data must be in .XPT format,
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
#' This function will return a list with each element corresponding to one wave of data.
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
                          dataPath=NULL,
                          write=FALSE,
                          extractAll=FALSE){

        if(!all(waves %in% LETTERS)) stop("One or more waves invalid. waves must be identified as a captial letter A-Z.")
        stopifnot(length(waves) >= 1)
        stopifnot(is.vector(waves))

        waves <- sort(waves)
        ## ensure ID variable is in search
        varnames <- unique(c('SEQN',varnames))
        ## find all files of .xpt structure that correspond to the year specified in the data
        if(is.null(dataPath)){
                dataPath <- system.file("extdat/covar/",package="nhanesdata")
        }
        files_full   <- list.files(dataPath)

        ret <-c()

        for(i in seq_along(waves)){
                cohort <- waves[i]

                pathExt <- paste0('_', cohort, '.XPT', sep='')
                ## find only those files associated with a particular NHANES wave
                files   <- files_full[substr(files_full, (nchar(files_full) - 5), nchar(files_full)) == pathExt]

                if(length(files) == 0) next

                ## find which files contain the variables requested by the user
                covarMats <- lapply(files, function(x){
                        mat <- try(read_xpt(paste0(dataPath,x)))
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
exclude_accel <- function(act, flags, threshold_lower = 600, threshold_upper = NULL, rm_PAXSTAT = TRUE, rm_PAXCAL = TRUE,
                          return_act = FALSE){
        stopifnot(all(is.data.frame(act), is.data.frame(flags)))
        stopifnot(all(colnames(act) == colnames(flags)))
        stopifnot(all(c("PAXSTAT", "PAXCAL", paste0("MIN",1:1440)) %in% colnames(act)))

        stopifnot(all(is.finite(act$PAXSTAT),is.finite(act$PAXCAL)))
        flag_nonwear <- rowSums(flags[, paste('MIN', 1:1440, sep='')], na.rm = TRUE) < threshold

        ## remove nonwear days and days flagged by NHANES
        cond <- c("flag_nonwear", "act$PAXSTAT!=1","act$PAXCAL!=1")[c(TRUE, rm_PAXSTAT, rm_PAXCAL)]
        cond <- paste(cond, collapse="|")

        if(return_act) return(eval(parse(text=paste("return(act[!(",cond, "),])"))))

        eval(parse(text=paste("return(which(!(",cond,")))")))
}




#' Reweight NHANES accelerometry data
#'
#' @description
#' This function re-weights accelerometry data for NHANES 2003-2004,2005-2006 waves
#'
#' @param data data frame to with survey weights to be re-weighted.Should only contain one subject per ror.
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
reweight_accel <- function(data, return_unadjusted_wts=TRUE){
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
        age_bks   <- c(0, 1, 3, 6, 12, 16, 20, 30, 40, 50, 60, 70, 80, 85, Inf)
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

        demo$age_cat_mn <- cut(demo$age_mn, breaks=age_bks, right=FALSE)
        demo$age_cat_ex <- cut(demo$age_ex, breaks=age_bks, right=FALSE)

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




