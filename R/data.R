#' A dataset containing minute level accelerometry data reported as "Activity Counts" for NHANES 2003-2004 participants selected for the Mobile Examination Center (MEC) portion of the study.
#'
#' @format A data frame with 50232 rows and 1445 variables. There are 7 rows per unqiue subject identifier (SEQN).
#'         Rows are ordered descending temporally within subjects
#'         (i.e. row 1 is the first day of data for the first participant, row 2 is the following calendar day, etc.).
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
#' @source \url{https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PAXRAW_C.htm}
"PAXINTEN_C"


#' A dataset containing minute level accelerometry data reported as "Activity Counts" for NHANES 2003-2004 participants selected for the Mobile Examination Center (MEC) portion of the study.
#'
#' @format A data frame with 52185 rows and 1445 variables. There are 7 rows per unqiue subject identifier (SEQN). Rows are ordered descending temporally within subjects (i.e. row 1 is the first day of data for the first participant, row 2 is the following calendar day, etc.).
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
#' @source \url{https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAXRAW_D.htm}
"PAXINTEN_D"




#' A dataset containing minute level wear non-wear flags for the NHANES 2003-2004 accelerometry data. The dimension and format of this dataset is the same as PAXINTEN_C except instead of reporting
#' activity counts in the MIN1-MIN1440 columns, we provide a wear/non-wear flag indicator.
#' These wear/non-wear flags were calculated by applying the algorithm described in Troiano et. al (2008) <doi:10.1249/mss.0b013e31815a51b3>.
#'
#' @format A data frame with 50232 rows and 1445 variables. There are 7 rows per unqiue subject identifier (SEQN). Rows are ordered descending temporally within subjects (i.e. row 1 is the first day of data for the first participant, row 2 is the following calendar day, etc.).
#' \itemize{
#'    \item{SEQN:}{ Unique subject identifier}
#'    \item{PAXCAL:}{ Device calibration.
#'    Was the device calibrated when it was returned by the participant? 1 = Yes, 2 = No, 9 = Don't Know.
#'    Any individuals with either 2 or 9 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{PAXSTAT:}{ Data reliability status flag. 1 = Data deemed reliable, 2 = Data reliability is questioable.
#'    Any individuals with 2 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{WEEKDAY:} { Day of the week: 1 = Sunday, 2 = Monday, 3 = Tuesday, 4 = Wednesday, 5 = Thursday, 6 = Friday, 7 = Saturday.}
#'    \item{SDDSRVYR:}{ Variable indicating which wave of the NHANES study this data is associated with. For example,
#'    SDDSRVYR = 3 corresponds to the 2003-2004 wave and SDDSRVYR = 4 corresponds to the 2005-2006 wave.}
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
"Flags_C"




#' A dataset containing minute level wear non-wear flags for the NHANES 2005-2006 accelerometry data. The dimension and format of this dataset is the same as PAXINTEN_D except instead of reporting
#' activity counts in the MIN1-MIN1440 columns, we provide a wear/non-wear flag indicator.
#' These wear/non-wear flags were calculated by applying the algorithm described in Troiano et. al (2008) <doi:10.1249/mss.0b013e31815a51b3>.
#'
#' @format A data frame with 52185 rows and 1445 variables. There are 7 rows per unqiue subject identifier (SEQN). Rows are ordered descending temporally within subjects (i.e. row 1 is the first day of data for the first participant, row 2 is the following calendar day, etc.).
#' \itemize{
#'    \item{SEQN:}{ Unique subject identifier}
#'    \item{PAXCAL:}{ Device calibration.
#'    Was the device calibrated when it was returned by the participant? 1 = Yes, 2 = No, 9 = Don't Know.
#'    Any individuals with either 2 or 9 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{PAXSTAT:}{ Data reliability status flag. 1 = Data deemed reliable, 2 = Data reliability is questioable.
#'    Any individuals with 2 in this variable should be examined carefully before being included in any analysis.
#'    }
#'    \item{WEEKDAY:} { Day of the week: 1 = Sunday, 2 = Monday, 3 = Tuesday, 4 = Wednesday, 5 = Thursday, 6 = Friday, 7 = Saturday.}
#'    \item{SDDSRVYR:}{ Variable indicating which wave of the NHANES study this data is associated with. For example,
#'    SDDSRVYR = 3 corresponds to the 2003-2004 wave and SDDSRVYR = 4 corresponds to the 2005-2006 wave.}
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
"Flags_D"






#' A dataset containing survey sampling data (e.g. survey weights, PSUs, strata) and a few select processed
#' demographic and lifestyle variables for NHANES 2003-2004 participant.
#'
#' @format A data frame with 10122 rows and 23 variables with one row per participant in the 2003-2004 wave.
#' \describe{
#'    \item{SEQN}{Unique subject identifier}
#'    \item{SDDSRVYR}{Numeric variable denoting NHANES wave. SDDSRVYR = 3 correpsonds to the 2003-2004 wave}
#'    \item{SDMVPSU}{Masked variance pseudo probability sampling units. Used for variance estimation.}
#'    \item{SDMVSTRA}{Masked variance pseudo stratum. Used for variance estimation.}
#'    \item{WTINT2YR}{Full sample interview weight.}
#'    \item{WTMEC2YR}{Full sample examinatin weight.}
#'    \item{RIDAGEMN}{Age in months at date of screening for individuals under the age of 85. Participants 85 and over are coded as NA.}
#'    \item{RIDAGEEX}{Age in months at examination (MEC) for individuals under the age of 85. Participants 85 and over are coded as NA.}
#'    \item{RIDAGEYR}{"Best" age in years at date of screening for individuals under the age of 85. Participants 85 and over are coded as 85. This variable is used when
#'    determining thresholds for questions using age inclusion/exclusion criteria}
#'    \item{BMI}{Body mass index (kg/m^2). This variable is a copy of the "BMXBMI" variable in the "Body Measures" data.}
#'    \item{BMI_cat}{Body mass index categorized into: underweight (<= 18.5), normal (> 18.5, <= 25), overweight (> 25, <= 30), and obese (> 30).}
#'    \item{Race}{Self reported ethnicity categorized into five levels: Mexican American, Other Hispanic, (Non-Hispanic) White, (Non-Hispanic) Black, and Other. This is a factor version of the variable "RIDRETH1" in the "Demographic Variables & Sample Weights" data.}
#'    \item{Gender}{Self reported gender categorized into Male and Female. This is a factor version of the variable "RIAGENDR" in the "Demographic Variables & Sample Weights" data.}
#'    \item{Diabetes}{Self reported doctor diagnosed diabetes (excluding gestational diabetes) categorized into: Yes, No, Borderline, Refused, Don't know. This is a factor version of the variable DIQ010 in the  "Diabetes" data.}
#'    \item{CHF}{Self reported medical professional diagnosed  congestive heart failure categorized into: Yes, No,  Refused, Don't know. This is a factor version of the variable MCQ160B in the  "Medical Conditions" data.}
#'    \item{CHD}{Self reported medical professional diagnosed  coronary heart disease categorized into: Yes, No,  Refused, Don't know. This is a factor version of the variable MCQ160C in the  "Medical Conditions" data.}
#'    \item{Cancer}{Self reported medical professional diagnosed history of any kind of cancer categorized into: Yes, No, Refused, Don't know. This is a factor version of the variable MCQ220 in the  "Medical Conditions" data.}
#'    \item{Stroke}{Self reported medical professional diagnosed history of stroke categorized into: Yes, No, Refused, Don't know. This is a factor version of the variable MCQ160F in the  "Medical Conditions" data.}
#'    \item{MobilityProblem}{Self reported mobility problem categorized into: No difficulty and Any difficulty.
#'    This variable is derived from the responses to questions: PFQ049, PFQ054, PFQ057, PFQ059, PFQ061B, and PFQ061C in the "Physical Functioning" data.
#'    If individuals reported any difficulty climbing up 10 stairs (PFQ061B) or walking a quarter mile (PFQ061C), they were classified as "Any difficulty" for this variable.
#'    If individuals reported that they did not perform either of these activities they were classified as "Any difficulty".
#'    If individuals reported that they required special equipment to walk, they were not asked PFQ061B/PFQ061C and were considered "Any difficulty" for this variable.
#'    Any indivdual who was 59 or younger and responded "No difficulty" to higher level physical functioning questions were not asked PFQ061B/PFQ061C and were considered "No difficulty" for this variable.}
#'    \item{DrinkStatus}{Current alcohol consumption status categorized into: Non-drinker, Moderate drinker, Heavy drinker. This variable is derived from several questionairre responses in the "Alcohol Use" data.
#'    Non-drinkers are identified as those individuals who either 1) responded "No" to whether they have had "at least 12 alcoholic drinks" in any one year,
#'    or over the course of their life (ALQ101, ALQ110); or 2) responded that they have had 0 drinks over the last 12 months (ALQ120Q).
#'    Moderate and heavy drinkers were identified using the CDC's gender specific thresholds of no more than 7 and 14 drinks per week for women and men, respectively.
#'    Drinks per week was calcualted using the data from questions ALQ120Q, ALQ120U, and ALQ130.
#'    Notes: 1) The number of drinks per week has some notable outliers. It may be that there was miscoding of the units (ALQ 120U) for some individuals' responses. We do not attempt any correction here.
#'    2) Heavy drinking here does not incorporate and information on binge drinking;
#'    3) This data is only publicly available for participants 20+ years old at the time of interview; and
#'    4) Any answer of "refused" or "don't know" was considered missing for ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130.}
#'    \item{DrinksPerWeek}{Self reported number of drinks per week based on responses to questions ALQ120Q, ALQ120U, and ALQ130.
#'    Individuals who responded "No" to to whether they have had "at least 12 alcoholic drinks" in any one year,
#'    or over the course of their life (ALQ101, ALQ110) were classified as 0 drinks per week.}
#'    \item{SmokeCigs}{Self reported cigarette smoking status categorized into: Never, Former, and Current.
#'    This variable is derived from responses to questions SMQ020 and SMQ040 in the "Smoking - Cigarette/Tobacco Use - Adult" data.
#'    We consider anyone who responds "No" to the question of whether they have ever smoked 100 cigarettes in their life (SMQ020) to be "Never" smokers.
#'    Former smokers are those individuals who respond "Yes" to having ever smoked 100 cigarettes in their life, but currently smoke "Not at all" (SMQ040).
#'    Current smokers are those individuals who both respond "Yes" to having ever smoked 100 cigarettes in their life and currently smoke either "Every day", or "Some days" (SMQ040).
#'    Note: 1) Any answer of "refused" or "don't know" was considered missing; and
#'    2) This data is only publicly available for participants 20+ years old at the time of interview.}
#' }
#'
#' @source \url{https://www.cdc.gov/nchs/nhanes/index.htm}
"Covariate_C"



#' A dataset containing survey sampling data (e.g. survey weights, PSUs, strata) and select processed
#' demographic and lifestyle variables for NHANES 2005-2006 participants.
#'
#' @format A data frame with 10348 rows and 23 variables with one row per participant in the 2005-2006 wave.
#' \describe{
#'    \item{SEQN}{Unique subject identifier}
#'    \item{SDDSRVYR}{Numeric variable denoting NHANES wave. SDDSRVYR = 3 correpsonds to the 2003-2004 wave}
#'    \item{SDMVPSU}{Masked variance pseudo probability sampling units. Used for variance estimation.}
#'    \item{SDMVSTRA}{Masked variance pseudo stratum. Used for variance estimation.}
#'    \item{WTINT2YR}{Full sample interview weight.}
#'    \item{WTMEC2YR}{Full sample examinatin weight.}
#'    \item{RIDAGEMN}{Age in months at date of screening for individuals under the age of 85. Participants over 85 are coded as NA.}
#'    \item{RIDAGEEX}{Age in months at examination (MEC) for individuals under the age of 85. Participants over 85 are coded as NA.}
#'    \item{RIDAGEYR}{"Best" age in years at date of screening for individuals under the age of 85. Participants 85 and over are coded as 85. This variable is used when
#'    determining thresholds for questions using age inclusion/exclusion criteria}
#'    \item{BMI}{Body mass index (kg/m^2). This variable is a copy of the "BMXBMI" variable in the "Body Measures" data.}
#'    \item{BMI_cat}{Body mass index categorized into: underweight (<= 18.5), normal (> 18.5, <= 25), overweight (> 25, <= 30), and obese (> 30).}
#'    \item{Race}{Self reported ethnicity categorized into five levels: Mexican American, Other Hispanic, (Non-Hispanic) White, (Non-Hispanic) Black, and Other. This is a factor version of the variable "RIDRETH1" in the "Demographic Variables & Sample Weights" data.}
#'    \item{Gender}{Self reported gender categorized into Male and Female. This is a factor version of the variable "RIAGENDR" in the "Demographic Variables & Sample Weights" data.}
#'    \item{Diabetes}{Self reported doctor diagnosed diabetes (excluding gestational diabetes) categorized into: Yes, No, Borderline, Refused, Don't know. This is a factor version of the variable DIQ010 in the  "Diabetes" data.}
#'    \item{CHF}{Self reported medical professional diagnosed  congestive heart failure categorized into: Yes, No,  Refused, Don't know. This is a factor version of the variable MCQ160B in the  "Medical Conditions" data.}
#'    \item{CHD}{Self reported medical professional diagnosed  coronary heart disease categorized into: Yes, No,  Refused, Don't know. This is a factor version of the variable MCQ160C in the  "Medical Conditions" data.}
#'    \item{Cancer}{Self reported medical professional diagnosed history of any kind of cancer categorized into: Yes, No, Refused, Don't know. This is a factor version of the variable MCQ220 in the  "Medical Conditions" data.}
#'    \item{Stroke}{Self reported medical professional diagnosed history of stroke categorized into: Yes, No, Refused, Don't know. This is a factor version of the variable MCQ160F in the  "Medical Conditions" data.}
#'    \item{MobilityProblem}{Self reported mobility problem categorized into: No difficulty and Any difficulty.
#'    This variable is derived from the responses to questions: PFQ049, PFQ054, PFQ057, PFQ059, PFQ061B, and PFQ061C in the "Physical Functioning" data.
#'    If individuals reported any difficulty climbing up 10 stairs (PFQ061B) or walking a quarter mile (PFQ061C), they were classified as "Any difficulty" for this variable.
#'    If individuals reported that they did not perform either of these activities they were classified as "Any difficulty".
#'    If individuals reported that they required special equipment to walk, they were not asked PFQ061B/PFQ061C and were considered "Any difficulty" for this variable.
#'    Any indivdual who was 59 or younger and responded "No difficulty" to higher level physical functioning questions were not asked PFQ061B/PFQ061C and were considered "No difficulty" for this variable.}
#'    \item{DrinkStatus}{Current alcohol consumption status categorized into: Non-drinker, Moderate drinker, Heavy drinker. This variable is derived from several questionairre responses in the "Alcohol Use" data.
#'    Non-drinkers are identified as those individuals who either 1) responded "No" to whether they have had "at least 12 alcoholic drinks" in any one year,
#'    or over the course of their life (ALQ101, ALQ110); or 2) responded that they have had 0 drinks over the last 12 months (ALQ120Q).
#'    Moderate and heavy drinkers were identified using the CDC's gender specific thresholds of no more than 7 and 14 drinks per week for women and men, respectively.
#'    Drinks per week was calcualted using the data from questions ALQ120Q, ALQ120U, and ALQ130.
#'    Notes: 1) The number of drinks per week has some notable outliers. It may be that there was miscoding of the units (ALQ 120U) for some individuals' responses. We do not attempt any correction here.
#'    2) Heavy drinking here does not incorporate and information on binge drinking;
#'    3) This data is only publicly available for participants 20+ years old at the time of interview; and
#'    4) Any answer of "refused" or "don't know" was considered missing for ALQ101, ALQ110, ALQ120Q, ALQ120U, ALQ130.}
#'    \item{DrinksPerWeek}{Self reported number of drinks per week based on responses to questions ALQ120Q, ALQ120U, and ALQ130.
#'    Individuals who responded "No" to to whether they have had "at least 12 alcoholic drinks" in any one year,
#'    or over the course of their life (ALQ101, ALQ110) were classified as 0 drinks per week.}
#'    \item{SmokeCigs}{Self reported cigarette smoking status categorized into: Never, Former, and Current.
#'    This variable is derived from responses to questions SMQ020 and SMQ040 in the "Smoking - Cigarette/Tobacco Use - Adult" data.
#'    We consider anyone who responds "No" to the question of whether they have ever smoked 100 cigarettes in their life (SMQ020) to be "Never" smokers.
#'    Former smokers are those individuals who respond "Yes" to having ever smoked 100 cigarettes in their life, but currently smoke "Not at all" (SMQ040).
#'    Current smokers are those individuals who both respond "Yes" to having ever smoked 100 cigarettes in their life and currently smoke either "Every day", or "Some days" (SMQ040).
#'    Note: 1) Any answer of "refused" or "don't know" was considered missing; and
#'    2) This data is only publicly available for participants 20+ years old at the time of interview.}
#' }
#'
#' @source \url{https://www.cdc.gov/nchs/nhanes/index.htm}
"Covariate_D"











#' A dataset containing processed publicly linked mortality data for NHANES 2003-2004 participants. This data corresponds to the 2011 release of the linked mortality data.
#' As new mortality data is released this data file will be updated.
#'
#' @format A data frame with 10122 rows and 14 variables
#' \describe{
#'    \item{SEQN}{Unique subject identifier}
#'    \item{eligstat}{Eligibility status for mortality follow-up
#'          \describe{
#'                  \item{1}{ Eligible}
#'                  \item{2}{ Under age 18, not available for public release}
#'                  \item{3}{ Ineligible}
#'          }
#'    }
#'    \item{mortat}{Indicator for whether participant was found to be alive or deceased at follow-up time given by
#'    permth_exm and permth_int
#'          \describe{
#'                  \item{0}{ Assumed alive}
#'                  \item{1}{ Assumed deceased}
#'                  \item{NA}{ Under age 18, not available for public release or ineligible for mortality follow-up}
#'          }
#'    }
#'    \item{permth_exm}{Time in months from the mobile examination center (MEC) assessment where mortality was assessed.}
#'    \item{permth_int}{Time in months from the household interview where mortality was assessed.}
#'    \item{ucod_leading}{Underlying cause of death recode from UCOD_113 leading causes where available. Specific causes:
#'            \describe{
#'                  \item{001}{ Diseases of the heart (I00-I09, I11, I13, I20-I51)}
#'                  \item{002}{ Malignant neoplasms (C00-C97)}
#'                  \item{003}{ Chronic lower respiratory diseases (J40-J47)}
#'                  \item{004}{ Accidents (unintentional injuries) (V01-X59, Y85-Y86)}
#'                  \item{005}{ Cerebrovascular diseases (I60-I69)}
#'                  \item{006}{ Alzheimer's disease (G30)}
#'                  \item{007}{ Diabetes mellitus (E10-E14)}
#'                  \item{008}{ Influenza and pneumonia (J09-J18)}
#'                  \item{009}{ Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)}
#'                  \item{010}{ All other causes (residual)}
#'                  \item{NA}{ Ineligible, under age 18, assumed alive or no cause data}
#'            }
#'      }
#'      \item{diabetes_mcod}{diabetes flag from multiple cause of death (mcod)}
#'      \item{hyperten_mcod}{hyperten flag from multiple cause of death (mcod)}
#'      \item{mortscrce_ndi}{mortality source: NDI match}
#'      \item{mortscrce_ssa}{mortality source: SSA information}
#'      \item{mortscrce_cms}{mortality source: CMS information}
#'      \item{mortscrce_dc}{mortality source: death certificate match}
#'      \item{mortscrce_dcl}{mortality source: data collection}
#'
#' }
#'
#' @source \url{https://www.cdc.gov/nchs/data-linkage/mortality-public.htm}
"Mortality_2011_C"



#' A dataset containing processed publicly linked mortality data for NHANES 2005-2006 participants. This data corresponds to the 2011 release of the linked mortality data.
#' As new mortality data is released this data file will be updated.
#'
#' @format A data frame with 10,348 rows and 14 variables
#'
#' \describe{
#'    \item{SEQN}{Unique subject identifier}
#'    \item{eligstat}{Eligibility status for mortality follow-up
#'          \describe{
#'                  \item{1}{ Eligible}
#'                  \item{2}{ Under age 18, not available for public release}
#'                  \item{3}{ Ineligible}
#'          }
#'    }
#'    \item{mortat}{Indicator for whether participant was found to be alive or deceased at follow-up time given by
#'    permth_exm and permth_int
#'          \describe{
#'                  \item{0}{ Assumed alive}
#'                  \item{1}{ Assumed deceased}
#'                  \item{NA}{ Under age 18, not available for public release or ineligible for mortality follow-up}
#'          }
#'    }
#'    \item{permth_exm}{Time in months from the mobile examination center (MEC) assessment where mortality was assessed.}
#'    \item{permth_int}{Time in months from the household interview where mortality was assessed.}
#'    \item{ucod_leading}{Underlying cause of death recode from UCOD_113 leading causes where available. Specific causes:
#'            \describe{
#'                  \item{001}{ Diseases of the heart (I00-I09, I11, I13, I20-I51)}
#'                  \item{002}{ Malignant neoplasms (C00-C97)}
#'                  \item{003}{ Chronic lower respiratory diseases (J40-J47)}
#'                  \item{004}{ Accidents (unintentional injuries) (V01-X59, Y85-Y86)}
#'                  \item{005}{ Cerebrovascular diseases (I60-I69)}
#'                  \item{006}{ Alzheimer's disease (G30)}
#'                  \item{007}{ Diabetes mellitus (E10-E14)}
#'                  \item{008}{ Influenza and pneumonia (J09-J18)}
#'                  \item{009}{ Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)}
#'                  \item{010}{ All other causes (residual)}
#'                  \item{NA}{ Ineligible, under age 18, assumed alive or no cause data}
#'            }
#'      }
#'      \item{diabetes_mcod}{diabetes flag from multiple cause of death (mcod)}
#'      \item{hyperten_mcod}{hyperten flag from multiple cause of death (mcod)}
#'      \item{mortscrce_ndi}{mortality source: NDI match}
#'      \item{mortscrce_ssa}{mortality source: SSA information}
#'      \item{mortscrce_cms}{mortality source: CMS information}
#'      \item{mortscrce_dc}{mortality source: death certificate match}
#'      \item{mortscrce_dcl}{mortality source: data collection}
#'
#' }
#'
#' @source \url{https://www.cdc.gov/nchs/data-linkage/mortality-public.htm}
"Mortality_2011_D"










