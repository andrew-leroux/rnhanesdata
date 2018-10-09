#' NHANES data package
#'
#' @docType package
#' @name rnhanesdata-package
#' @aliases rnhanesdata
#'
#'
#'
#' @description
#'
#' This package contains tools for processing the NHANES 2003-2004 and 2005-2006 waves' accelerometry data. Specifically, the code
#' contained herein will transform the "long format" accelerometry data which is available for download from the CDC, to the 1440+ format
#' with one row per subject-day, calculate wear/non-wear flags on the tranformed data, merge NHANES data from non-accelerometry sources, and
#' process national mortality data linked to NHANES participants. In addition to code for processing the 2003-2006 accerlerometry data,
#' this package contains code to: process and merge NHANES data across multiple "raw" (.xpt) data files which are downloaded from the NHANES website; and
#' process linked mortality data for the NHANES 2003-2006 participants.
#'
#'
#'
#'
#' @details
#'
#'
#' This primary contents of this package are: 1) four data procesing functions and two functions helpful in analyzing NHANES data;
#' 2) processed accelerometry, demographic, lifestyle, comorbiditiy, and mortality data for the NHANES 2003-2006 waves; and
#' 3) raw data used to create the processed data in the package, excluding the ``raw" accelerometer data due to package file size limitaitons.
#' These three sets of content are described in more detail below.
#'
#' \itemize{
#'     \item{Data processing functions:}{ The data processing functions used here can be used to reproduce all the data files in this package. See the package vignette on processing the NHANES
#'     accelerometry data for details on how exactly to reproduce all data in the package using the four functions below.
#'
#'     In general, these functions work by specifying the NHANES waves which you wish to process, as well as additional function-specific parameters. The NHANES waves
#'     are specified using the NHANES naming convention of "Name"_*.ext where "Name" denotes file name, \* corresponds to the letter associated with a particular NHANES wave
#'     (for example, _C and _D correspond to the 2003-2004 and 2005-2006 waves, respectively), and .ext indicates file extension.
#'           \itemize{
#'                 \item{\code{\link{process_accel}}}{ The function \code{\link{process_accel} will take the long-format accelerometry data which is downloaded directly from the CDC website, and transform it to the 1440+ format
#'                 with one row per subejct-day, and seven rows per subject. This can either 1) process data that has been downlaoded locally; or 2) downlaod the data from the CDC. See the package vignette
#'                 for an example of how to download}
#'                 \item{\code{\link{process_flags}}}{The function \code{\link{process_flags}} will take the 1440+ format accelerometry data which has been created by the \code{\link{process_accel}} function.
#'                 This function uses the \code{\link{weartime}} to estimate weartime with a 90-minute window and tolerance for up to 2 minutes of activity counts less than 100 in a non-wear interval.
#'                 See \code{\link{weartime}} for details on weartime algorithm parameters. }
#'                 \item{\code{\link{process_covar}}}{The function \code{\link{process_covar}} is intented to make retrieving and merging data across many different .xpt files easy.
#'                 Specificlly, the fucntion will take a vector of variable names associated with NHANES data and search all SAS xport (.xpt) files  that match
#'                 the NHANES naming convention in a directory for the variables specified.}
#'                 This function has the option to not specify any variables, and instead return every variable across all files. }
#'                 \item{\code{\link{process_mort}}}{The function \code{\link{process_mort}} will take the raw .dat mortality files linked to NHANES participants (downloadable from the CDC) and
#'                 transform them into a format compatable with analyses.}
#'            }
#'     }
#'     \item{Data analysis functions:}{
#'           \itemize{
#'                 \item{\code{\link{reweight_accel}}}{ The function reweight_accel will calculate 2- and 4-year survey weights for NHANES 2003-2004 and 2005-2006 waves on a dataset containing participants
#'                 from both waves. The function has the option to return both adjusted and unadjusted survey weights. This function calculates adjusted and unadjusted interview and examination survey weights.
#'                 The adjusted survey weights are calculated based on a missing completely at random  assumption within age, gender, and ethinicity strata (see \code{\link{reweight_accel}} for additional details).}
#'                 \item{\code{\link{exclude_accel}}}{ The function exclude_accel will identify days which meet minimum wear-time criteria and are deemed as "good" by on the basis of NHANES supplied
#'                 data qualify flags (PAXSTAT) and device calibration indicator (PAXCAL).}
#'            }
#'     }
#' }
#'
#' The processed data included in the package are broadly divided into four categories:
#' 1) activity count data; 2) wear/non-wear flags; 3) additional NHANES data; 4) linked mortality data.
#'
#' \itemize{
#'     \item{Activity count data}{
#'           \itemize{
#'                 \item{\code{\link{PAXINTEN_C}}}{ Accelerometry data for the NHANES 2003-2004 wave}
#'                 \item{\code{\link{PAXINTEN_D}}}{ Accelerometry data for the NHANES 2005-2006 wave}
#'           }
#'     }
#'     \item{Wear/non-wear flags}{
#'           \itemize{
#'                 \item{\code{\link{Flags_C}}}{ Wear/non-wear flags estimated using the \code{\link{process_flags}} function applied to the 2003-2004 accelerometry data (\code{\link{PAXINTEN_C}})}
#'                 \item{\code{\link{Flags_D}}}{ Wear/non-wear flags estimated using the \code{\link{process_flags}} function applied to the 2005-2006 accelerometry data (\code{\link{PAXINTEN_D}})}
#'           }
#'     }
#'     \item{Additional NHANES data}{
#'           \itemize{
#'                 \item{\code{\link{Covariate_C}}}{ Processed versions of commonly used demographic, lifestyle, and comordbiditiy data for the NHANES 2003-2004 wave}
#'                 \item{\code{\link{Covariate_D}}}{ Processed versions of commonly used demographic, lifestyle, and comordbiditiy data for the NHANES 2005-2006 wave}
#'           }
#'     }
#'     \item{Linked mortality data}{
#'           \itemize{
#'                 \item{\code{\link{Mortality_2011_C}}}{ Processed mortality data for the NHANES 2003-2004 wave. Mortality data corresponds to the 2011 release.}
#'                 \item{\code{\link{Mortality_2011_D}}}{ Processed mortality data for the NHANES 2005-2006 wave. Mortality data corresponds to the 2011 release.}
#'           }
#'     }
#' }
#'
#' Finally, this package also contains raw NHANES data (excluding accelerometry) and the 2011 publicly linked mortality data. These data are not publicly facing and therefore do not have
#' documentation. However, all variables contained in the "raw NHANES data" files listed below are the \code{\link{process_covar}} function. In addition, the
#' \code{\link{process_mort}} uses the raw "linked mortality data" to create the processed mortality data included in the package.
#'
#' \itemize{
#'      \item{Raw NHANES data}{
#'            \itemize{
#'                \item{ALQ_C.XPT and ALQ_D.XPT}{ Alcohol consumption questionairre data for the 2003-2004 and 2005-2006 waves, respectively.}
#'                \item{BMX_C.XPT and BMX_D.XPT}{ Body measurement data for the 2003-2004 and 2005-2006 waves, respectively.}
#'                \item{BPX_C.XPT and BPX_D.XPT}{ Blood pressure data for the 2003-2004 and 2005-2006 waves, respectively.}
#'                \item{DEMO_C.XPT and DEMO_D.XPT}{ Demographic data for the 2003-2004 and 2005-2006 waves, respectively.}
#'                \item{DIQ_C.XPT and DIQ_D.XPT}{ Diabetes questionairre data for the 2003-2004 and 2005-2006 waves, respectively.}
#'                \item{MCQ_C.XPT and MCQ_D.XPT}{ Medical conditions questionairre data for the 2003-2004 and 2005-2006 waves, respectively.}
#'                \item{PFQ_C.XPT and PFQ_D.XPT}{ Physical function questionairre data for the 2003-2004 and 2005-2006 waves, respectively.}
#'                \item{SMQ_C.XPT and SMQ_D.XPT}{ Smoking status questionairre data for the 2003-2004 and 2005-2006 waves, respectively.}
#'            }
#'      }
#'      \item{Linked mortality data}{
#'           \itemize{
#'                \item{NHANES_2003_2004_MORT_2011_PUBLIC.dat}{ Raw mortality data for the 2003-2004 wave}
#'                \item{NHANES_2005_2006_MORT_2011_PUBLIC.dat}{ Raw mortality data for the 2005-2006 wave}
#'           }
#'      }
#'
#' }
#'
#'
#' @author
#'
#' Andrew Leroux <aleroux2@jhu.edu>
#'
#' Ciprian Crainiceanu
#'
#' @references
#'
#'
NULL





## quiets concerns of R CMD check for reweight_accel
if(getRversion() >= "2.15.1")  utils::globalVariables(c("Covariate_C","Covariate_D"))



