#' @title Diagnosis of Metabolic Dysfunction Associated Fatty Liver Disease
#'
#' @description The latest guidelines proposed by International Expert Consensus are used widely for the clinical diagnosis of Metabolic Associated Fatty Liver Disease (MAFLD). The new definition takes hepatic steatosis (determined by elastography or histology or biomarker-based fatty liver index) as a major criterion. In addition, race, gender, body mass index (BMI), waist circumference (WC), fasting plasma glucose (FPG), systolic blood pressure (SBP), diastolic blood pressure (DBP), triglycerides (TG), high-density lipoprotein cholesterol (HDLC), homeostatic model assessment of insulin resistance (HOMAIR), high sensitive c-reactive protein (HsCRP) for the diagnosis of MAFLD. Each parameter has to be interpreted based on the proposed cut-offs, making the diagnosis slightly complex and error-prone. This package is developed by incorporating the latest international expert consensus guidelines, and it will aid in the easy and quick diagnosis of MAFLD based on FibroScan in busy healthcare settings and also for research purposes. The new definition for MAFLD as per the International Consensus Statement is described by Eslam M et al (2020). <doi:10.1016/j.jhep.2020.03.039>.

#' @param x a data frame with column names as exactly specified.
#'
#' @return Yes or No
#' @export
#' @importFrom dplyr case_when
#'
#' @examples
#' MAFLD(x)


MAFLD <- function(x){case_when(x$CAP >= 272 &
                                (x$BMI >= 25 & x$Race=="Caucasians")~'Yes',
                                (x$BMI >= 23 & x$Race=="Asians")~'Yes',
                                (x$FPG >= 126 |x$HbA1C >= 6.5 ) ~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                (x$WC > 102 & x$SBP >= 130 | x$DBP >= 85 | x$HDL < 40 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                (x$SBP >= 130 & x$WC > 102 | x$DBP >= 85 | x$HDL < 40 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$DBP >= 85 & x$WC > 102 |x$SBP >= 130   | x$HDL < 40 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HDL < 40  & x$WC > 102 |x$SBP >= 130|x$DBP >= 85 |  x$TG >= 100 |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$TG >= 100 & x$WC > 102 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$FPG >=100  & x$WC > 102 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HbA1C >=5.7 & x$WC > 102 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$FPG >=100 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HOMAIR >=2.5  & x$WC > 102 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HSCRP >2 & x$WC > 102 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HOMAIR >=2.5))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$WC > 88 & x$SBP >= 130 | x$DBP >= 85 | x$HDL < 50 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$SBP >= 130 & x$WC > 88 | x$DBP >= 85 | x$HDL < 50 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$DBP >= 85 & x$WC > 88 |x$SBP >= 130   | x$HDL < 50 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HDL < 50  & x$WC > 88 |x$SBP >= 130|x$DBP >= 85 |  x$TG >= 100 |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$TG >= 100 & x$WC > 88 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$FPG >=100  & x$WC > 88 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HbA1C >=5.7 & x$WC > 88 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$FPG >=100 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HOMAIR >=2.5  & x$WC > 88 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HSCRP >2 & x$WC > 88 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HOMAIR >=2.5))~'Yes',
                                (x$BMI < 23 & x$Race=="Asians"  & x$Gender =="Male" &
                                   (x$WC > 90 & x$SBP >= 130 | x$DBP >= 85 | x$HDL < 40 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$SBP >= 130 & x$WC > 90 | x$DBP >= 85 | x$HDL < 40 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$DBP >= 85 & x$WC > 90 |x$SBP >= 130   | x$HDL < 40 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HDL < 40  & x$WC > 90 |x$SBP >= 130|x$DBP >= 85 |  x$TG >= 100 |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$TG >= 100 & x$WC > 90 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$FPG >=100  & x$WC > 90 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HbA1C >=5.7 & x$WC > 90 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$FPG >=100 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HOMAIR >=2.5  & x$WC > 90 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Male" &
                                   (x$HSCRP >2 & x$WC > 90 |x$SBP >= 130|x$DBP >= 85 |  x$HDL < 40  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HOMAIR >=2.5))~'Yes',
                                (x$BMI < 23 & x$Race=="Asians"  & x$Gender =="Female" &
                                   (x$WC > 80 & x$SBP >= 130 | x$DBP >= 85 | x$HDL < 50 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$SBP >= 130 & x$WC > 80| x$DBP >= 85 | x$HDL < 50 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$DBP >= 85 & x$WC > 80|x$SBP >= 130   | x$HDL < 50 | x$TG >= 100 | x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HDL < 50  & x$WC > 80|x$SBP >= 130|x$DBP >= 85 |  x$TG >= 100 |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$TG >= 100 & x$WC > 80|x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$FPG >=100 |x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$FPG >=100  & x$WC > 80|x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$HbA1C >=5.7 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HbA1C >=5.7 & x$WC > 80|x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$FPG >=100 |x$HOMAIR >=2.5 |x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HOMAIR >=2.5  & x$WC > 80|x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HSCRP >2))~'Yes',
                                (x$BMI < 25 & x$Race=="Caucasians"  & x$Gender =="Female" &
                                   (x$HSCRP >2 & x$WC > 80|x$SBP >= 130|x$DBP >= 85 |  x$HDL < 50  |x$TG >= 100|x$FPG >=100 |x$HbA1C >=5.7|x$HOMAIR >=2.5))~'Yes',
                                TRUE ~ 'No')}

