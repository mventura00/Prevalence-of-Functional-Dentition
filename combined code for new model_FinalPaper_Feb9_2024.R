#Prevalence-of-Functional-Dentition
# By Marta Ventura, Paul Griffin, and Susan Griffin (Jan 15 , 2024), Penn State University, Dept of Industrial & Mfg Engr
# email for Marta Ventura:  mxv176@psu.edu


#install.packages("installr")
#library(installr)
#updateR()

#Set working directory
#setwd("C:/Users/mxv17/OneDrive/Desktop/R_for_BRFSS")

#check working directory
#getwd()

library(dplyr)
library(haven)
library(survey)
library(tidyverse)

library(jtools)
library(ggplot2)
library("plotrix")


BRFSS_12 <- read_sas("C:/Users/mxv17/OneDrive/Desktop/R_for_BRFSS/LLCP2012.sas7bdat")
BRFSS_18 <- read_sas("C:/Users/mxv17/OneDrive/Desktop/R_for_BRFSS/llcp2018.sas7bdat")

options(survey.lonely.psu = "adjust")


#mutation

BRFSS_12 <- mutate(BRFSS_12,survyear = 2012)
BRFSS_18 <- mutate(BRFSS_18,survyear = 2018)




# 2012 MISSING TEETH calcuations
BRFSS_12 <- mutate(BRFSS_12,
                   MT = case_when(
                     RMVTETH3 == 8 ~ 1,  ## No Missing teeth
                     RMVTETH3 == 1 ~ 2,  # Missing  1 to 5 teeth
                     RMVTETH3 == 2 ~ 3,  # Missing 6 or more, but not all
                     RMVTETH3 == 3 ~ 4,  # All teeth missing
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, RMVTETH3, MT)                               


BRFSS_12 <- mutate(BRFSS_12,
                   misteeth = case_when(
                     MT == 1 ~ 1,  # NO MISSING TEETH
                     MT == 2 ~ 2,   # Functional Dentition (missing 1-5 teeth)
                     MT %in% c(3, 4) ~ 3,   # Not Functional Dentition
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, MT, misteeth)

count(BRFSS_12, misteeth)

count(BRFSS_12, MT, misteeth)

BRFSS_12 <- mutate(BRFSS_12,
                   funcdent = case_when(
                     MT %in% c(1,2) ~ 1,  #FUNCTIONAL DENTITION
                     MT %in% c(3, 4) ~ 0, # NOT Functional Dentition
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, MT, funcdent)
count(BRFSS_12, funcdent)


# 2018 MISSING TEETH calcuations
BRFSS_18 <- mutate(BRFSS_18,
                   MT = case_when(
                     RMVTETH4 == 8 ~ 1, #NO missing teeth
                     RMVTETH4 == 1 ~ 2, # 1-5 missing teeth
                     RMVTETH4 == 2 ~ 3, # More than 5 but not all
                     RMVTETH4 == 3 ~ 4, # All missing teeth
                     TRUE ~ NA_real_
                   ))
count(BRFSS_18, RMVTETH4, MT)    

table(BRFSS_18$RMVTETH4)

BRFSS_18 <- mutate(BRFSS_18,
                   MT = case_when(
                     RMVTETH4 == 8 ~ 1,
                     RMVTETH4 == 1 ~ 2,
                     RMVTETH4 == 2 ~ 3,
                     RMVTETH4 == 3 ~ 4,
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, RMVTETH4, MT)    

BRFSS_18 <- mutate(BRFSS_18,
                   misteeth = case_when(
                     MT == 1 ~ 1,  # NO MISSING TEETH
                     MT == 2 ~ 2,   # Functional Dentition
                     MT %in% c(3, 4) ~ 3,   # Not Functional Dentition
                     TRUE ~ NA_real_
                   ))
count(BRFSS_18, MT, misteeth)

BRFSS_18 <- mutate(BRFSS_18,
                   funcdent = case_when(
                     MT %in% c(1,2) ~ 1, #Functional Dentiion mixxing 0 - 5 teeth
                     MT %in% c(3, 4) ~ 0,  #NOT functional dentition
                     TRUE ~ NA_real_
                   ))
count(BRFSS_18, funcdent)


BRFSS_12 <- mutate(BRFSS_12,
                   CB_0718 = case_when(
                     `_STATE` %in% c(1, 4, 10, 12, 13, 16, 23, 24, 28, 29, 32, 33, 40, 47, 48, 49, 54) ~ 2,  #NEVER exposed (REFERENCE VARIABLE)
                     `_STATE` %in% c(2,5,9,11,17, 18,19,21,22,25,26,27,30,31,34,35,36,37,38,39, 41,42,44,46,50,53,55) ~ 3, #ALWAYS Exposed
                     `_STATE` %in% c(6,8,20,45,51,56) ~ 1,  #GAINED EXPOSURE (REFERENCE)
                     `_STATE` %in% c(15) ~ 1,  #LOST EXPOSURE (REFERENCE)
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_STATE`, CB_0718)
count(BRFSS_12, CB_0718)
table1<-count(BRFSS_12, `_STATE`,CB_0718)
#view(table1)

BRFSS_18 <- mutate(BRFSS_18,
                   CB_0718 = case_when(
                     `_STATE` %in% c(1, 4, 10, 12, 13, 16, 23, 24, 28, 29, 32, 33, 40, 47, 48, 49, 54) ~ 2,  #NEVER exposed
                     `_STATE` %in% c(2,5,9,11,17, 18,19,21,22,25,26,27,30,31,34,35,36,37,38,39, 41,42,44,46,50,53,55) ~ 3, #Always Exposed aka KEPT
                     `_STATE` %in% c(6,8,20,45,51,56) ~ 1, #Gained exposure
                     `_STATE` %in% c(15) ~ 1,  #Lost exposure  #HAWAII 
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, `_STATE`, CB_0718)
count(BRFSS_18, CB_0718)

table<-count(BRFSS_18, `_STATE`, CB_0718)

#print(n=53,table)


BRFSS_12 <- mutate(BRFSS_12,
                   CB_1218 = case_when(
                     `_STATE` %in% c(1, 4, 5, 8, 10, 11, 12, 13, 15, 16, 17, 18, 20, 21, 22, 27, 24, 26, 27, 28, 29, 30, 31, 32, 33, 40, 42, 45, 46, 47, 48, 49, 50, 51, 54) ~ 1,  #Emergency benefits, NEVER had Compreh Benefits (REFERENCE VARIABLE)
                     `_STATE` %in% c(2,6, 9, 19, 25, 34, 35, 36, 37, 38, 39, 41, 44, 53, 55) ~ 2, #ALWAYS had Compreh Benefits
                      TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_STATE`, CB_1218)

BRFSS_18 <- mutate(BRFSS_18,
                   CB_1218 = case_when(
                     `_STATE` %in% c(1, 4, 5, 8, 10, 11, 12, 13, 15, 16, 17, 18, 20, 21, 22, 27, 24, 26, 27, 28, 29, 30, 31, 32, 33, 40, 42, 45, 46, 47, 48, 49, 50, 51, 54) ~ 1,  #Emergency benefits, NEVER had Compre Benefits =Emergencyt
                     `_STATE` %in% c(2,6, 9, 19, 25, 34, 35, 36, 37, 38, 39, 41, 44, 53, 55) ~ 2, #Always had Compreh Benefits
                     TRUE ~ NA_real_
                   ))
                 
count(BRFSS_18, `_STATE`, CB_1218)


BRFSS_12 <- mutate(BRFSS_12,
                   educate = case_when(
                     `_EDUCAG` == 1 ~ 2,  # < than HS
                     `_EDUCAG` == 2 ~ 3,  #only HS
                     `_EDUCAG` %in% c(3,4) ~ 1,  # > than HS
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_EDUCAG`, educate)

BRFSS_18 <- mutate(BRFSS_18,
                   educate = case_when(
                     `_EDUCAG` == 1 ~ 2,  # < than HS
                     `_EDUCAG` == 2 ~ 3,  #only HS
                     `_EDUCAG` %in% c(3,4) ~ 1,  # > than HS
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, `_EDUCAG`, educate)

BRFSS_12 <- mutate(BRFSS_12,
                   agecat = case_when(
                     #      `_AGEG5YR` == 2 ~ 2, 
                     #      `_AGEG5YR` == 3 ~ 3,
                     `_AGEG5YR` == 4 ~ 2,  #AGED 35 - 39
                     `_AGEG5YR` == 5 ~ 3,  #AGED 40 - 44
                     `_AGEG5YR` == 6 ~ 4,  #OLDER PEOPLE  45 - 49 (all group into one category)
                     `_AGEG5YR` == 7 ~ 1,  #OLDER PEOPLE  50 - 54 (all group into one category)
                     #`_AGEG5YR` == 8 ~ 1,  #OLDER PEOPLE   55 - 59  (all group into one category)
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_AGEG5YR`, agecat)


BRFSS_12 <- mutate(BRFSS_12,
                   agecat_more = case_when(
                     #      `_AGEG5YR` == 2 ~ 2,
                     #      `_AGEG5YR` == 3 ~ 3,
                     `_AGEG5YR` == 4 ~ 2,   #AGED 35 - 39
                     `_AGEG5YR` == 5 ~ 3,   #AGED 40 - 44
                     `_AGEG5YR` == 6 ~ 4,   #OLDER PEOPLE  45-49 yrs  
                     `_AGEG5YR` == 7 ~ 5,   #OLDER PEOPLE  50-54 yrs  
                     `_AGEG5YR` == 8 ~ 6,   #OLDER PEOPLE  55-59 yrs  
                     `_AGEG5YR` == 9 ~ 7,   #OLDER PEOPLE  60-64 yrs  
                     `_AGEG5YR` == 10 ~ 8,  #OLDER PEOPLE 65-69 yrs (all group into one category)
                     `_AGEG5YR` == 11 ~ 9,  #OLDER PEOPLE  70-74 yrs (all group into one category)
                     `_AGEG5YR` == 12 ~ 10, #OLDER PEOPLE  75-79 yrs (all group into one category)
                     `_AGEG5YR` == 13 ~ 11, #OLDER PEOPLE  80+ yrs (all group into one category)
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_AGEG5YR`, agecat_more)


BRFSS_18 <- mutate(BRFSS_18,
                   agecat = case_when(
                     #      `_AGEG5YR` == 2 ~ 2,
                     #      `_AGEG5YR` == 3 ~ 3,
                     `_AGEG5YR` == 4 ~ 2,   #AGED 35 - 39
                     `_AGEG5YR` == 5 ~ 3,   #AGED 40 - 44
                     `_AGEG5YR` == 6 ~ 4,   #OLDER PEOPLE  45 - 49 (all group into one category)
                     `_AGEG5YR` == 7 ~ 1,   #OLDER PEOPLE  50 - 54 (all group into one category)
                     #`_AGEG5YR` == 8 ~ 1,   #OLDER PEOPLE  55 - 59 (all group into one category)
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, `_AGEG5YR`, agecat)


BRFSS_18 <- mutate(BRFSS_18,
                   agecat_more = case_when(
                     #      `_AGEG5YR` == 2 ~ 2,
                     #      `_AGEG5YR` == 3 ~ 3,
                     `_AGEG5YR` == 4 ~ 2,   #AGED 35 - 39
                     `_AGEG5YR` == 5 ~ 3,   #AGED 40 - 44
                     `_AGEG5YR` == 6 ~ 4,   #OLDER PEOPLE  45-49 yrs  
                     `_AGEG5YR` == 7 ~ 5,   #OLDER PEOPLE  50-54 yrs  
                     `_AGEG5YR` == 8 ~ 6,   #OLDER PEOPLE  55-59 yrs  (all group into one category)
                     `_AGEG5YR` == 9 ~ 7,   #OLDER PEOPLE  60-64 yrs  (all group into one category)
                     `_AGEG5YR` == 10 ~ 8,  #OLDER PEOPLE 65-69 yrs (all group into one category)
                     `_AGEG5YR` == 11 ~ 9,  #OLDER PEOPLE  70-74 yrs (all group into one category)
                     `_AGEG5YR` == 12 ~ 10, #OLDER PEOPLE  75-79 yrs (all group into one category)
                     `_AGEG5YR` == 13 ~ 11, #OLDER PEOPLE  80+ yrs (all group into one category)
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, `_AGEG5YR`, agecat_more)


BRFSS_12 <- mutate(BRFSS_12,
                   male = case_when(
                     SEX == 1 ~ 2, # MALE 
                     SEX == 2 ~ 1, # FEMALE (reference)
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, SEX, male)



BRFSS_18 <- mutate(BRFSS_18,
                   male = case_when(
                     SEX1 == 1 ~ 2, #males
                     SEX1 == 2 ~ 1, #female (reference)
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, SEX1, male)


BRFSS_12 <- mutate(BRFSS_12,
                   smoker = case_when(
                     `_SMOKER3` %in% c(1,2) ~ 2,  # CURRENT smoker (Reference variable)
                     `_SMOKER3` == 3 ~ 3,  # Former smoker (they used to smoke)
                     `_SMOKER3` == 4 ~ 1,  # NEVER smoked
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_SMOKER3`, smoker)


BRFSS_18 <- mutate(BRFSS_18,
                   smoker = case_when(
                     `_SMOKER3` %in% c(1,2) ~ 2,  # CURRENT smoker (Reference variable) - smokes some days, or everyday 
                     `_SMOKER3` == 3 ~ 3,         # Former smoker (they used to smoke)
                     `_SMOKER3` == 4 ~ 1,         # NEVER smoked
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, `_SMOKER3`, smoker)



BRFSS_12 <- mutate(BRFSS_12,
                   raceethn = case_when(
                     `_RACE_G` == 1 ~ 2,  # White Not-Hispanic
                     `_RACE_G` == 2 ~ 3,  # Black  Not-Hispanic
                     `_RACE_G` == 3 ~ 4,  # Hispanic
                     `_RACE_G` %in% c(4,5) ~ 1,  # Other Multi
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_RACE_G`, raceethn)


BRFSS_18 <- mutate(BRFSS_18,
                   raceethn = case_when(
                     `_RACE_G1` == 1 ~ 2,  # White Not-Hispanic
                     `_RACE_G1` == 2 ~ 3,  # Black  Not-Hispanic
                     `_RACE_G1` == 3 ~ 4,  # Hispanic
                     `_RACE_G1` %in% c(4,5) ~ 1,  # Other Multi
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, `_RACE_G1`, raceethn)



BRFSS_12 <- mutate(BRFSS_12,
                   poorhlth = case_when(
                     GENHLTH %in% c(4,5) ~ 2,  # Poor Health
                     GENHLTH %in% c(1,2,3) ~ 1,  #Excellent/Good health
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, GENHLTH, poorhlth)


BRFSS_18 <- mutate(BRFSS_18,
                   poorhlth = case_when(
                     GENHLTH %in% c(4,5) ~ 2, # Poor Health
                     GENHLTH %in% c(1,2,3) ~ 1, #Excellent/Good health
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, GENHLTH, poorhlth)

#INCOME TO POVERTY RATIO CALCULAT for 2012!!!!
BRFSS_12 <- mutate(BRFSS_12,
                   parent = case_when(
                     CHILDREN >= 1 & CHILDREN <= 87 ~ 1,
                     CHILDREN == 88 ~ 0, TRUE ~ NA_real_))

parentcount<-count(BRFSS_12, parent, CHILDREN) 

BRFSS_12 <- mutate(BRFSS_12,
                   NUM = case_when(
                     MARITAL == 1 ~ 2 + CHILDREN,
                     MARITAL > 1 | MARITAL <= 6 ~ 1 + CHILDREN,
                     MARITAL ==9 ~ 1 + CHILDREN, TRUE ~ NA_real_))

count(BRFSS_12, MARITAL, NUM)     

BRFSS_12 <- mutate(BRFSS_12,
                   income = case_when(
                     `_INCOMG` == 1 ~ 7500,
                     `_INCOMG` == 2 ~ 20000,
                     `_INCOMG` == 3 ~ 30000,
                     `_INCOMG` == 4 ~ 42500,
                     `_INCOMG` == 5 ~ 50000,
                     TRUE ~ NA_real_
                   ))

count(BRFSS_12, `_INCOMG`, income)

BRFSS_12 <- mutate(BRFSS_12,
                   hhscom = case_when(
                     NUM == 1 ~ 11170,  # Federal Poverty 
                     NUM == 2 ~ 15130,
                     NUM == 3 ~ 19090,
                     NUM == 4 ~ 23050,
                     NUM == 5 ~ 27010,
                     NUM == 6 ~ 30970,
                     NUM == 7 ~ 34930,
                     NUM == 8 ~ 38890,
                     NUM >= 9 ~ 38890+(3960*(NUM-8)),TRUE ~ NA_real_))

BRFSS_12 <- mutate(BRFSS_12,
                   pov = income/hhscom) 

BRFSS_12 <- mutate(BRFSS_12,
                   pov138two = case_when(
                     pov <= 1.33 ~ 2,   # QUALIFY FOR MEDICAID : 138% * FPL . = 1.38*11170 = 15,414 , for 1 member
                     pov > 1.33 ~ 1, TRUE ~ NA_real_))  # NOT qualified for Medicaid




#INCOME TO POVERTY RATIO CALCULAT for 2018!!!!

BRFSS_18 <- mutate(BRFSS_18,
                   income = case_when(
                     `_INCOMG` == 1 ~ 7500,
                     `_INCOMG` == 2 ~ 20000,
                     `_INCOMG` == 3 ~ 30000,
                     `_INCOMG` == 4 ~ 42500,
                     `_INCOMG` == 5 ~ 50000,
                     TRUE ~ NA_real_
                   ))

count(BRFSS_18, `_INCOMG`, income)

BRFSS_18 <- mutate(BRFSS_18,
                   parent = case_when(
                     CHILDREN >= 1 & CHILDREN <= 87 ~ 1,
                     CHILDREN == 88 ~ 0, TRUE ~ NA_real_))

count(BRFSS_18, parent, CHILDREN) 


BRFSS_18 <- mutate(BRFSS_18,
                   NUM = case_when(
                     MARITAL == 1 ~ 2 + CHILDREN,
                     MARITAL > 1 | MARITAL <= 6 ~ 1 + CHILDREN,
                     MARITAL ==9 ~ 1 + CHILDREN, TRUE ~ NA_real_))

count(BRFSS_18, MARITAL, NUM) 

BRFSS_18 <- mutate(BRFSS_18,
                   hhscom = case_when(
                     NUM == 1 ~ 12140,
                     NUM == 2 ~ 16460,
                     NUM == 3 ~ 20780,
                     NUM == 4 ~ 25100,
                     NUM == 5 ~ 29420,
                     NUM == 6 ~ 33740,
                     NUM == 7 ~ 38060,
                     NUM == 8 ~ 42380,
                     NUM >= 9 ~ 42380+(4320*(NUM-8)),TRUE ~ NA_real_))

BRFSS_18 <- mutate(BRFSS_18,
                   pov = income/hhscom) 

count(BRFSS_18, pov)  

BRFSS_18 <- mutate(BRFSS_18,
                   pov138two = case_when(
                     pov <= 1.38 ~ 2,  #Medicaid
                     pov > 1.38 ~ 1, TRUE ~ NA_real_))  #NOT Medicaid




BRFSS_12 = drop_na(BRFSS_12, MT,  funcdent, misteeth, agecat, agecat_more, educate, survyear, parent, pov138two, CB_1218,
                   CB_0718, educate, smoker, poorhlth, male, raceethn, `_STATE`, `_LLCPWT`, `_STSTR`, `_PSU`)
BRFSS_18 = drop_na(BRFSS_18, MT,  funcdent, misteeth, agecat, agecat_more, educate, survyear, parent, pov138two, CB_1218,
                   CB_0718, educate, smoker, poorhlth, male, raceethn, `_STATE`, `_LLCPWT`, `_STSTR`, `_PSU`)


total <- rbind(select(BRFSS_12, funcdent, misteeth, agecat, agecat_more, educate, survyear, parent, pov138two, 
                      educate, smoker, poorhlth, male, raceethn, `_STATE`, CB_1218, CB_0718, `_LLCPWT`, `_STSTR`, `_PSU`), 
               select(BRFSS_18, funcdent, misteeth, agecat, agecat_more, educate, survyear, parent, pov138two, 
                      educate, smoker, poorhlth, male, raceethn, `_STATE`, CB_1218, CB_0718, `_LLCPWT`, `_STSTR`, `_PSU`))
count(total)
count(total$survyear==2012)

#factor varaibles
total$survyear<-as.factor(total$survyear)
total$agecat<-as.factor(total$agecat)
total$male<-as.factor(total$male)
total$smoker<-as.factor(total$smoker)
total$poorhlth<-as.factor(total$poorhlth)
total$educate<-as.factor(total$educate)
total$raceethn<-as.factor(total$raceethn)
total$`_STATE`<-as.factor(total$`_STATE`)
total$CB_1218<-as.factor(total$CB_1218)
total$CB_0718<-as.factor(total$CB_0718)
total$pov138two<-as.factor(total$pov138two)

total$agecat_more<-as.factor(total$agecat_more)

count(total,CB_0718)
count(total,CB_1218)

levels(total$CB_0718)
levels(total$survyear)
# Change the reference level to '6'
total$survyear <- relevel(total$survyear, ref = "2018")

levels(total$survyear)

total$survyear<-as.factor(total$survyear)
levels(total$survyear)

design1 <- svydesign(id = ~1, strata = ~`_STSTR`, weights = ~`_LLCPWT`, data = total)  # COMPLEX SURVEY
design1

#total$l
count(BRFSS_18, `_AGEG5YR`, agecat_more)

## MEDICAID ELIGIBLE
#MEDICAID , both years
filter1<-filter(total, pov138two ==2)  # NOT Medicaid, 2012 & 2018
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2012)  # Medicaid, 2012 
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2012, CB_0718==2)  # Medicaid, 2012 , NEVER had CB
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2012, CB_0718==3)  # Medicaid, 2012 , ALWAYS had CB
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2012, CB_0718==1)  # Medicaid, 2012 , 'Gained' or 'Lost'CB
summary(filter1)
count(filter1)


#2018 year, MEDICAID
#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2018)  # Medicaid,   2018
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2018, CB_0718==2)  # Medicaid, 2018 , NEVER had CB
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2018, CB_0718==3)  # Medicaid, 2018 , ALWAYS had CB
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==2,survyear==2018, CB_0718==1)  # Medicaid, 2018 , 'Gained' or 'Lost'CB
summary(filter1)
count(filter1)


#NOT Medicaid eligible for both 2012 and 2018
filter1<-filter(total, pov138two ==1)  # NOT Medicaid, 2012 & 2018
summary(filter1)
count(filter1)

#2012 year, NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2012)  # NOT Medicaid, 2012 
summary(filter1)
count(filter1)

#NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2012, CB_0718==2)  # NOT Medicaid, 2012 , NEVER had CB
summary(filter1)
count(filter1)

#NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2012, CB_0718==3)  # NOT Medicaid, 2012 , ALWAYS had CB
summary(filter1)
count(filter1)

#NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2012, CB_0718==1)  # NOT Medicaid, 2012 , 'Gained' or 'Lost'CB
summary(filter1)
count(filter1)


#2018 year, NOT Medicaid

#NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2018)  # NOT Medicaid,   2018
summary(filter1)
count(filter1)

#NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2018, CB_0718==2)  # NOT Medicaid, 2018 , NEVER had CB
summary(filter1)
count(filter1)

#NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2018, CB_0718==3)  # NOT Medicaid, 2018 , ALWAYS had CB
summary(filter1)
count(filter1)

#NOT Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2018, CB_0718==1)  # NOT Medicaid, 2018 , 'Gained' or 'Lost'CB
summary(filter1)
count(filter1)















# STANDARD ERROR CALCULATION!!!!
# consider a vector with 10 elements
a <- c(179, 160, 136, 227, 123, 23,
        45, 67, 1, 234)

# calculate standard error
print(sd(a)/sqrt(length((a))))


# calculate standard error using in built 
# function
print(std.error(a))



# BIVARIATE RESULTS -- needs to use data WITHOUT missing values! 
#Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2012)  # NOT Medicaid, 2012 
summary(filter1)
count(filter1)

#Medicaid eligible
filter1<-filter(total, pov138two ==1,survyear==2012)  # NOT Medicaid, 2012 , WITH COMPREHENSIVE BENEFITS
summary(filter1)
count(filter1)
count(filter1, agecat==1)






# MEDICAID qualified group

filter_sg1 <- filter(total, survyear==2012 & pov138two ==1 & CB_1218== 2 )   # Comprehensive, NOT Medicaid, 2012   
summary(filter_sg1)

summary(filter(total, survyear==2018 & pov138two ==1 & CB_1218== 2 ))   # Comprehensive, NOT Medicaid, 2018  


summary(filter(total, survyear==2012 & pov138two ==1 & CB_1218== 1 ))  # NOT COMPREHENSIVE, NOT Medicaid, 2012 & educate==1)


summary(filter(total, survyear==2018 & pov138two ==1 & CB_1218== 1 ))  # NOT COMPREHENSIVE, NOT Medicaid, 2018  & educate==1


# NOT MEDICAID qualified group for AGE GROUP BUCKETS!
summary(filter(total, survyear==2012 & pov138two ==2 & CB_1218== 2 ))  #  Comprehensive,  Medicaid, 2012 


summary(filter(total, survyear==2018 & pov138two ==2 & CB_1218== 2 ))  # Comprehensive,  Medicaid, 2018


summary(filter(total, survyear==2012 & pov138two ==2 & CB_1218== 1 ))  #NOT COMPREHENSIVE,  Medicaid, 2012 


summary(filter(total, survyear==2018 & pov138two ==2 & CB_1218== 1 ))  #NOT COMPREHENSIVE,  Medicaid, 2018 






filter1<-filter(total , survyear==2012 & pov138two ==1 & CB_1218== 2 )  #& agecat_more==2  
summary(filter1)







 #model_1  (OLD MODEL)

# model_1 <- glm(funcdent ~ pov138two + CB_0718 + survyear + pov138two*CB_0718  + survyear*pov138two + CB_0718*survyear 
#                + survyear*CB_0718*pov138two + agecat + male + smoker + educate + poorhlth + raceethn +`_STATE`, data = total, family = binomial(link = "logit"))
# 
# summary(model_1)

levels(BRFSS_18$CB_0718) 
count(total,CB_0718)



#DF <- within(DF, b<- relevel(b, ref))

# Fit the linear model with the new reference level
#model_new <- lm(mpg ~ as.factor(survyear), data = total)

# MODEL 1 - SURVEY DESIGN!!!!!  PART A
# DESIGN 1

model1 <- svyglm(as.factor(funcdent) ~ 
                   pov138two  +
                   CB_1218+
                   survyear+
                   pov138two*CB_1218+
                   survyear*pov138two +
                   CB_1218*survyear +
                   survyear*CB_1218*pov138two +
                   agecat +
                   male +
                   smoker +
                   educate +
                   poorhlth +
                   raceethn + 
                   `_STATE`, 
                 design = design1, family=binomial)


summary(model1)



      
odds_ratio <- exp(model1$coefficients)
odds_ratio

odds_ratio <- exp(coef(model1))
odds_ratio


prob <- odds_ratio/(1 + odds_ratio)
prob

names(total)



##count(total%>% (survyear==2012 & CB_1218 == 2 & pov138two ==2 )%>% agecat_more)

# MODEL 1 - SURVEY DESIGN!!!!!  PART B
# DESIGN 1
model1_b <- svyglm(as.factor(funcdent) ~ 
                   pov138two  +
                   CB_1218 +
                   survyear +
                   pov138two*CB_1218 +
                   survyear*pov138two +
                   CB_1218*survyear +
                   survyear*CB_1218*pov138two +
                   agecat_more +
                   male +
                   smoker +
                   educate +
                   poorhlth +
                   raceethn + 
                   `_STATE`, 
                 design = design1, family=binomial)


summary(model1_b)


odds_ratio <- exp(coef(model1_b))
odds_ratio

prob <- odds_ratio/(1 + odds_ratio)
prob

 


# MODEL 2 - LOOK HERE!!
             
model_2 <- svyglm(as.factor(funcdent) ~ 
                    CB_0718 + 
                    survyear + 
                    CB_0718: survyear +
                    agecat + 
                    male + 
                    smoker + 
                    educate + 
                    poorhlth + 
                    raceethn +
                    `_STATE`, 
               design = design1,
               family = binomial)

summary(model_2)


total[total$CB_0718==3 & total$survyear]


cb_2_2012<-filter(total, CB_0718==1, survyear==2012)  #lots of data, tibble 94,032 x 17
cb_2_2018<-filter(total, CB_0718==1, survyear==2018)  #lots of data, tibble 70,365 x 17

mean(cb_2_2012$funcdent)


cb_2_2012<-filter(total, CB_0718==2, survyear==2012)  #lots of data, tibble 94,032 x 17
cb_2_2018<-filter(total, CB_0718==2, survyear==2018)  #lots of data, tibble 70,365 x 17

cb_3_2012<-filter(total, CB_0718==3, survyear==2012) #NONE show up, it has a tibble: of 0x17 matrix (tibble is a pkg in R used to manipuate and print df. A tibble is the latest method for reimagining a dataset )
cb_3_2018<-filter(total, CB_0718==3, survyear==2018)  # lots show up, a tibble of 17,303x 17 matrix

cb_4_2012<-filter(total, CB_0718==4, survyear==2012) #NONE show up, it has a tibble of 0x17 matrix
cb_4_2018<-filter(total, CB_0718==4, survyear==2018)  # lots show up, a tibble of 2,560 x17 matrix



odds_ratio_2 <- exp(coef(model_2))
odds_ratio_2


prob <- odds_ratio_2/(1 + odds_ratio_2)
prob


# MODEL 3 - LOOK HERE!!
View(total)
levels(total$survyear)

model_3 <- svyglm(as.factor(funcdent) ~ CB_0718 + survyear + pov138two
               + CB_0718: survyear 
               + CB_0718:pov138two
               + survyear:pov138two
               + CB_0718:survyear:pov138two
               + agecat + male + smoker + educate + poorhlth + raceethn +`_STATE`, 
               design = design1, 
               family = binomial)

summary(model_3)


#summ(model_3)
#interact_plot(model_3, pred = CB_0718, modx = survyear)

total[total$CB_0718==3 & total$survyear]

cb_2_pov138two1<-filter(total, CB_0718==2, pov138two==1)  #lots of data, tibble 94,032 x 17
cb_2_pov138two2<-filter(total, CB_0718==2, pov138two==2)  #lots of data, tibble 70,365 x 17

cb_3_pov138two1<-filter(total, CB_0718==3, pov138two==1)   
cb_3_pov138two2<-filter(total, CB_0718==3, pov138two==2) 

cb_4_pov138two1<-filter(total, CB_0718==4, pov138two==1)   
cb_4_pov138two2<-filter(total, CB_0718==4, pov138two==2) 


odds_ratio_3 <- exp(coef(model_3))
odds_ratio_3

levels(total$pov138two)

prob <- odds_ratio_3/(1 + odds_ratio_3)
prob


# DIFFERENCE IN DIFFERENCE!!!!



model1 <- svyglm(as.factor(funcdent) ~ 
                   pov138two  +
                   CB_1218 +
                   survyear +
                   pov138two*CB_1218 +
                   survyear*pov138two +
                   CB_1218*survyear +
                   survyear*CB_1218*pov138two +
                   agecat +
                   male +
                   smoker +
                   educate +
                   poorhlth +
                   raceethn + 
                   `_STATE`, 
                 design = design1, family=binomial)


#MODEL 1 - DID probabilities!!!   DID NOT DROP MISSING VALUES (I COMMENTED those lines out)
# LEFT HAND SIDE
data1<-filter(total,pov138two == 2, CB_0718 == 2, survyear==2012)  # LOW INCOME, Never had CB, 2012
n1<-nrow(data1)
p1<-n1/nrow(total) 
p1

count(data1,funcdent,agecat, is.na=F)  # getting counts for different AGE groups!!
nrow(data1)

# DATA 2
data2<-filter(total,pov138two == 2, CB_0718 == 2, survyear==2018)  # LOW INCOME, Never had CB, 2018
n2<-nrow(data2)
p2<-n2/nrow(total)
p2

count(data2,funcdent,agecat, is.na=F)
nrow(data2)

# DATA 3
data3<-filter(total,pov138two == 1, CB_0718 == 2, survyear==2012)  #HIGH income, Never had CB, 2012
n3<-nrow(data3)
p3<-n3/nrow(total)
p3

count(data3,funcdent,agecat, is.na=F)
nrow(data3)

# DATA 4
data4<-filter(total,pov138two == 1, CB_0718 == 2, survyear==2018)  #HIGH income, Never had CB, 2018
n4<-nrow(data4)
p4<-n4/nrow(total)
p4
count(data4,funcdent, agecat, is.na=F)
nrow(data4)

# RIGHT HAND SIDE

# DATA 5
data5<-filter(total,pov138two == 2, CB_0718 == 3, survyear==2012)  # LOW INCOME, ALWAYS had CB, 2012
n5<-nrow(data5)
p5<-n5/nrow(total)
p5

count(data5,funcdent, agecat, is.na=F)
nrow(data5)

# DATA 6
data6<-filter(total,pov138two == 2, CB_0718 == 3, survyear==2018)  # LOW INCOME, ALWAYS had CB, 2018
n6<-nrow(data6)
p6<-n6/nrow(total)
p6

count(data6,funcdent, agecat, is.na=F)
nrow(data6)

# DATA 7

data7<-filter(total,pov138two == 1, CB_0718 == 3, survyear==2012)  #HIGH income, ALWAYS had CB, 2012
n7<-nrow(data7)
p7<-n7/nrow(total)
p7

count(data7,funcdent, agecat,is.na=F)
nrow(data7)

# DATA 8
data8<-filter(total,pov138two == 1, CB_0718 == 3, survyear==2018)  #HIGH income, ALWAYS had CB, 2018
n8<-nrow(data8)
p8<-n8/nrow(total)
p8

count(data8,funcdent,agecat, is.na=F)
nrow(data8)

## This is GAINED CB
# DATA 9
data5<-filter(total,pov138two == 2, CB_0718 == 3, survyear==2012)  # LOW INCOME, ALWAYS had CB, 2012
n5<-nrow(data5)
p5<-n5/nrow(total)
p5

count(data5,funcdent, agecat, is.na=F)
nrow(data5)

# DATA 10
data6<-filter(total,pov138two == 2, CB_0718 == 3, survyear==2018)  # LOW INCOME, ALWAYS had CB, 2018
n6<-nrow(data6)
p6<-n6/nrow(total)
p6

count(data6,funcdent, agecat, is.na=F)
nrow(data6)

# DATA 11

data7<-filter(total,pov138two == 1, CB_0718 == 3, survyear==2012)  #HIGH income, ALWAYS had CB, 2012
n7<-nrow(data7)
p7<-n7/nrow(total)
p7

count(data7,funcdent, agecat,is.na=F)
nrow(data7)

# DATA 12
data8<-filter(total,pov138two == 1, CB_0718 == 3, survyear==2018)  #HIGH income, ALWAYS had CB, 2018
n8<-nrow(data8)
p8<-n8/nrow(total)
p8

count(data8,funcdent,agecat, is.na=F)
nrow(data8)




# create proportion variable first..... TO CREATE DID , NEED TO DO THIS (for MODEL 1)
prop <- c(p1,p2,p3,p4,p5,p6,p7,p8)

surveyyear <- c(2012,2018,2012,2018,2012,2018,2012,2018)
surveyyearcode<-c(0,1,0,1,0,1,0,1)
pov138tw0_DID <- c(1,1,0,0,1,1,0,0)
CB_1218_DID <- c(1,0,1,0,1,0,1,0)
funcDent_DID <- c(1,1,1,1,1,1,1,1)

# create dataframe
data <-data.frame( cbind(prop, surveyyearcode, pov138tw0_DID, CB_0718_DID, funcDent_DID))
data
plot(prop)
barplot((prop))
?barplot
barplot(formula=prop,horiz = FALSE, xlab = porportion, ylab = density)


barplot(formula=prop, horiz = FALSE,
        height = data$prop, 
        main = "DID", 
        xlab = "Proportion", 
        ylab = "Density", 
        #names.arg = IB$Browser,
        border = "dark blue", 
        col = "pink")



# MODEL #2 - Proportions and DID Calculations
model_2 <- svyglm(as.factor(funcdent) ~ 
                    CB_0718   + survyear + CB_0718 : survyear+
                    agecat + 
                    male + 
                    smoker + 
                    educate + 
                    poorhlth + 
                    raceethn +
                    `_STATE`, 
                  design = design1,
                  family = binomial)

summary(model_2)

#MODEL 2  DID!!!!
# LEFT HAND SIDE SET of COLUMNS
data1<-filter(total,CB_0718 == 1, survyear==2012)  # Never Exposed to CB, 2012,
n1<-nrow(data1)
p1<-n1/nrow(total)
p1

count(data1,funcdent,is.na=F)
nrow(data1)


data2<-filter(total, CB_0718 == 1, survyear==2018)  # Never Exposed to CB, 2018, has Functional Dentition
n2<-nrow(data2)
p2<-n2/nrow(total)
p2

count(data2,funcdent,is.na=F)
nrow(data2)

data3<-filter(total, CB_0718 == 2, survyear==2012)  # Always Exposed to CB, 2012, has Functional Dentition
n3<-nrow(data3)
p3<-n3/nrow(total)
p3

count(data3,funcdent,is.na=F)
nrow(data3)


data4<-filter(total, CB_0718 == 2, survyear==2018)  #Always Exposed to CB, 2018, has Functional Dentition
n4<-nrow(data4)
p4<-n4/nrow(total)
p4

count(data4,funcdent,is.na=F)
nrow(data4)

# RIGHT HAND SIDE SET of COLUMNS
data5<-filter(total, CB_0718 == 3, survyear==2012)  # Gained Exposure to CB, 2012, has Functional Dentition
n5<-nrow(data5)
p5<-n5/nrow(total)
p5

count(data5,funcdent,is.na=F)
nrow(data5)

data6<-filter(total, CB_0718 == 3, survyear==2018)  # Gained Exposure to CB, 2018, has Functional Dentition
n6<-nrow(data6)
p6<-n6/nrow(total)
p6

count(data6,funcdent,is.na=F)
nrow(data6)

data7<-filter(total, CB_0718 == 4, survyear==2012)  # Lost Exposure to CB, 2012, has Functional Dentition
n7<-nrow(data7)
p7<-n7/nrow(total)
p7

count(data7,funcdent,is.na=F)
nrow(data7)


data8<-filter(total, CB_0718 == 4, survyear==2018)  # Lost Exposure to CB, 2012, has Functional Dentition
n8<-nrow(data8)
p8<-n8/nrow(total)
p8

count(data8,funcdent,is.na=F)
nrow(data8)

# create proportion variable first..... TO CREATE DID , NEED TO DO THIS (for MODEL 1)
prop <- c(p1,p2,p3,p4,p5,p6,p7,p8)

surveyyear <- c(2012,2018,2012,2018,2012,2018,2012,2018)
surveyyearcode<-c(0,1,0,1,0,1,0,1)
pov138tw0_DID <- c(1,1,0,0,1,1,0,0)
CB_0718_DID <- c(1,0,1,0,1,0,1,0)
funcDent_DID <- c(1,1,1,1,1,1,1,1)

# create dataframe
data <-data.frame( cbind(prop, surveyyearcode, pov138tw0_DID, CB_0718_DID, funcDent_DID))
data
plot(prop)
barplot((prop))
?barplot
barplot(formula=prop,horiz = FALSE, xlab = porportion, ylab = density)


barplot(formula=prop, horiz = FALSE,
        height = data$prop, 
        main = "DID", 
        xlab = "Proportion", 
        ylab = "Density", 
        #names.arg = IB$Browser,
        border = "dark blue", 
        col = "pink")


# MODEL #3 - Proportions and DID Calculations
model_3 <- svyglm(as.factor(funcdent) ~ CB_0718 + survyear + pov138two
                  + CB_0718: survyear 
                  + CB_0718:pov138two
                  + survyear:pov138two
                  + CB_0718:survyear:pov138two
                  + agecat + male + smoker + educate + poorhlth + raceethn +`_STATE`, 
                  design = design1, 
                  family = binomial)

summary(model_3)


#MODEL 3  DID !! Last model!!  DO THIS DID RESULTS FOR MODEL #3 -- and PLOTS!!FOR PAPER!!!

# "NEVER Exposed" compreh benefits

data1<-filter(total, CB_0718 == 2, pov138two == 2, survyear==2012)  # NEVER Exposed to CB, LOW Income (MEDICAID), 2012
n1<-nrow(data1)
p1<-n1/nrow(total)
p1

count(data1,funcdent,agecat, is.na=F) # getting counts for different AGE groups!!
nrow(data1)


data2<-filter(total, CB_0718 == 2, pov138two == 2, survyear==2018)  #NEVER Exposed to CB, LOW Income (MEDICAID), 2018
n2<-nrow(data2)
p2<-n2/nrow(total)
p2

count(data2,funcdent,agecat, is.na=F)
nrow(data2)


data3<-filter(total, CB_0718 == 2, pov138two == 1, survyear==2012)  # NEVER Exposed to CB, High Income (NON-MEDICAID), 2012
n3<-nrow(data3)
p3<-n3/nrow(total)
p3

count(data3,funcdent,agecat,is.na=F)
nrow(data3)

data4<-filter(total, CB_0718 == 2, pov138two == 1, survyear==2018)  # NEVER Exposed to CB, High Income (NON-MEDICAID), 2018
n4<-nrow(data2)
p4<-n4/nrow(total)
p4

count(data4,funcdent,agecat,is.na=F)
nrow(data4)




## "ALWAYS had" compreh benefits - Bar Graps DID
data5<-filter(total, CB_0718 == 3, pov138two == 2, survyear==2012)  # ALWAYS had CB, LOW Income, 2012,
n5<-nrow(data5)
p5<-n5/nrow(total)
p5

count(data5,funcdent,agecat,is.na=F)
nrow(data5)


data6<-filter(total, CB_0718 == 3, pov138two == 2, survyear==2018)  # ALWAYS had CB, Low Income, 2018,
n6<-nrow(data6)
p6<-n6/nrow(total)
p6

count(data6,funcdent,agecat,is.na=F)
nrow(data6)

data7<-filter(total, CB_0718 == 3, pov138two == 1, survyear==2012)  # ALWAYS had CB, High Income,  2012, 
n7<-nrow(data7)
p7<-n7/nrow(total)
p7

count(data7,funcdent,agecat,is.na=F)
nrow(data7)


data8<-filter(total, CB_0718 == 3, pov138two == 1, survyear==2018)  # ALWAYS had CB, High Income, 2018, 
n8<-nrow(data8)
p8<-n8/nrow(total)
p8

count(data8,funcdent,agecat,is.na=F)
nrow(data8)




# "GAINED/ LOST" CB, Bar Graps DID
data9<-filter(total, CB_0718 == 1, pov138two == 2, survyear==2012)  # GAINED/LOST Exposure to CB, LOW INCOME, 2012
n9<-nrow(data9)
p9<-n9/nrow(total)
p9

count(data9,funcdent, agecat, is.na=F)
nrow(data9)

data10<-filter(total, CB_0718 == 1, pov138two == 2, survyear==2018)  # GAINED/LOST Exposure to CB,  LOW INCOME, 2018
n10<-nrow(data10)
p10<-n10/nrow(total)
p10

count(data10,funcdent, agecat, is.na=F)
nrow(data10)

data11<-filter(total, CB_0718 == 1, pov138two == 1, survyear==2012)  # GAINED/LOST Exposure to CB, HIGH INCOME, 2012, has Functional Dentition
n11<-nrow(data11)
p11<-n11/nrow(total)
p11

count(data11,funcdent, agecat, is.na=F)
nrow(data11)

data12<-filter(total, CB_0718 == 1, pov138two == 1, survyear==2018)  # GAINED/LOST Exposure to CB, HIGH INCOME, 2018, has Functional Dentition
n12<-nrow(data12)
p12<-n12/nrow(total)
p12

count(data12,funcdent, agecat, is.na=F)
nrow(data12)


# create proportion variable first..... TO CREATE DID , NEED TO DO THIS (for MODEL 1)
prop <- c(p1,p2,p3,p4,p5,p6,p7,p8)

surveyyear <- c(2012,2018,2012,2018,2012,2018,2012,2018)
surveyyearcode<-c(0,1,0,1,0,1,0,1)
pov138two_DID <- c(1,1,0,0,1,1,0,0)
CB_1218_DID <- c(1,0,1,0,1,0,1,0)
funcDent_DID <- c(1,1,1,1,1,1,1,1)

# create dataframe
data <-data.frame( cbind(prop, surveyyearcode, parent_DID, expMed_DID, funcDent_DID))
data

