#' @title Diagnosis of Metabolic Dysfunction Associated Fatty Liver Disease
#'
#' @description The modified Adult Treatment Panel -III guidelines (ATP-III) proposed by American Heart Association (AHA) and National Heart, Lung and Blood Institute (NHLBI) are used widely for the clinical diagnosis of Metabolic Syndrome. The AHA-NHLBI criteria advise using parameters such as waist circumference (WC), systolic blood pressure (SBP), diastolic blood pressure (DBP), fasting plasma glucose (FPG), triglycerides (TG) and high-density lipoprotein cholesterol (HDLC) for diagnosis of metabolic syndrome. Each parameter has to be interpreted based on the proposed cut-offs, making the diagnosis slightly complex and error-prone. This package is developed by incorporating the modified ATP-III guidelines, and it will aid in the easy and quick diagnosis of metabolic syndrome in busy healthcare settings and also for research purposes. The modified ATP-III-AHA-NHLBI criteria for the diagnosis is described by Grundy et al ., (2005) <doi:10.1161/CIRCULATIONAHA.105.169404>.
#'
#' @param x a data frame with column names as exactly specified.
#'
#' @return Yes or No
#' @export
#' @importFrom dplyr case_when
#'
#' @examples
#' MAFLD(x)


MAFLD <- function(x) {case_when(CAP >= 272 &
                                  (BMI >= 25 & Race=="Caucasians" )~'Yes',
                                (BMI >= 23 & Race=="Asians")~'Yes',
                                (FPG >= 126 |HbA1C >= 6.5 ) ~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (WC > 102 & SBP >= 130 | DBP >= 85 | HDL < 40 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (SBP >= 130 & WC > 102 | DBP >= 85 | HDL < 40 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (DBP >= 85 & WC > 102 |SBP >= 130   | HDL < 40 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HDL < 40  & WC > 102 |SBP >= 130|DBP >= 85 |  TG >= 100 |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (TG >= 100 & WC > 102 |SBP >= 130|DBP >= 85 |  HDL < 40  |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (FPG >=100  & WC > 102 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HbA1C >=5.7 & WC > 102 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|FPG >=100 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HOMAIR >=2.5  & WC > 102 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|FPG >=100 |HbA1C >=5.7|HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HSCRP >2 & WC > 102 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|FPG >=100 |HbA1C >=5.7|HOMAIR >=2.5))~'Yes',

                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (WC > 88 & SBP >= 130 | DBP >= 85 | HDL < 50 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (SBP >= 130 & WC > 88 | DBP >= 85 | HDL < 50 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (DBP >= 85 & WC > 88 |SBP >= 130   | HDL < 50 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HDL < 50  & WC > 88 |SBP >= 130|DBP >= 85 |  TG >= 100 |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (TG >= 100 & WC > 88 |SBP >= 130|DBP >= 85 |  HDL < 50  |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (FPG >=100  & WC > 88 |SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HbA1C >=5.7 & WC > 88 |SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|FPG >=100 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HOMAIR >=2.5  & WC > 88 |SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|FPG >=100 |HbA1C >=5.7|HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HSCRP >2 & WC > 88 |SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|FPG >=100 |HbA1C >=5.7|HOMAIR >=2.5))~'Yes',

                                (BMI < 23 & Race=="Asians"  & Gender =="Male" &
                                   (WC > 90 & SBP >= 130 | DBP >= 85 | HDL < 40 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (SBP >= 130 & WC > 90 | DBP >= 85 | HDL < 40 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (DBP >= 85 & WC > 90 |SBP >= 130   | HDL < 40 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HDL < 40  & WC > 90 |SBP >= 130|DBP >= 85 |  TG >= 100 |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (TG >= 100 & WC > 90 |SBP >= 130|DBP >= 85 |  HDL < 40  |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (FPG >=100  & WC > 90 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HbA1C >=5.7 & WC > 90 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|FPG >=100 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HOMAIR >=2.5  & WC > 90 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|FPG >=100 |HbA1C >=5.7|HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Male" &
                                   (HSCRP >2 & WC > 90 |SBP >= 130|DBP >= 85 |  HDL < 40  |TG >= 100|FPG >=100 |HbA1C >=5.7|HOMAIR >=2.5))~'Yes',

                                (BMI < 23 & Race=="Asians"  & Gender =="Female" &
                                   (WC > 80 & SBP >= 130 | DBP >= 85 | HDL < 50 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (SBP >= 130 & WC > 80| DBP >= 85 | HDL < 50 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (DBP >= 85 & WC > 80|SBP >= 130   | HDL < 50 | TG >= 100 | FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HDL < 50  & WC > 80|SBP >= 130|DBP >= 85 |  TG >= 100 |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (TG >= 100 & WC > 80|SBP >= 130|DBP >= 85 |  HDL < 50  |FPG >=100 |HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (FPG >=100  & WC > 80|SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|HbA1C >=5.7 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HbA1C >=5.7 & WC > 80|SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|FPG >=100 |HOMAIR >=2.5 |HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HOMAIR >=2.5  & WC > 80|SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|FPG >=100 |HbA1C >=5.7|HSCRP >2))~'Yes',
                                (BMI < 25 & Race=="Caucasians"  & Gender =="Female" &
                                   (HSCRP >2 & WC > 80|SBP >= 130|DBP >= 85 |  HDL < 50  |TG >= 100|FPG >=100 |HbA1C >=5.7|HOMAIR >=2.5))~'Yes',
                                TRUE ~ 'No')}
