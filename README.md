# Prevalence-of-Functional-Dentition
By Marta Ventura, Paul Griffin, and Susan Griffin (Jan 15 , 2024)

This is our R-code for the paper we are submitting to the CDC for clearance and then to the Journal of Dental Research. 

We used data from the Behavioral Risk Factor Surveillance Survey (BRFSS) for the years 2007 to 2018.  Note that BRFSS uses multistage sampling.  We therefore performed our analysis in R-Studio (v.4.3.0) to account for the oversampling and multistage clustering used in the survey (used the package 'survey' to say what kind of survey BRFSS uses)

Methods 

Data Source

We used data from the Behavioral Risk Factor Surveillance Survey (BRFSS) for the years 2007 to 2018.  Note that BRFSS uses multistage sampling.  We therefore performed our analysis in R-Studio (v.4.3.0) to account for the oversampling and multistage clustering used in the survey.  Medicaid-eligible individuals were stratified by age (35 to 39, 40 to 44, 45 to 49, and 50 to 54), education (less than high school, high school, more than high school), sex (female, male), race/ethnicity (White non-Hispanic, Black non-Hispanic, Hispanic, other), smoking history (current smoker, former smoker, or never have smoked), and general health status (fair/poor, or excellent/very good/good).  We also included the state where the individual lived.

Study Design and Model Parameters

We used a difference in difference in difference (DID) model through ordinal logistic regression to examine the impact of changing dental benefits from a state’s Medicaid policy on tooth retention for Medicaid eligible results (25 to 54 years of age). Medicaid Assistance eligibility is determined using income and household size in comparison to income limits set by the 2023 Poverty Guidelines for persons/ household and poverty guideline. For our dataset, we could find the person’s family income and the number of people in their household, and we also matched it to the corresponding ‘income limit’ (federal poverty line (FPL)) . We divided the person’s income for household by the ‘income limit’ (FPL) and if it is less or equal to 1.33, then they qualify for Medicaid. If not, then they do not qualify for Medicaid. The exposure variable is an individual’s Medicaid benefits over time. Specifically, the exposure is defined by: i) no benefits in either period 2007-2012 and 2013- 2018 (i.e., never exposed (Never CB)) , ii) continuous benefits in both periods 2007-2012 and 2013- 2018 (i.e., Always CB); iii) no benefits in 2007-2012 and benefits in 2013-2018 (i.e., Gained CB), and iv) benefits in 2007-2012 and no benefits in 2013-2018 (i.e., Lost CB).  There were 14 states in the ‘Never CB category’ (Arizona, Florida, Georgia, Idaho, Maine, Maryland, Mississippi, Missouri, Nevada, New Hampshire, Oklahoma, Texas, Utah, and West Virginia), 27 states in the ‘Always CB category’ (Alaska, Arkansas, Connecticut, District of Columbia (D.C.), Illinois, Indiana, Iowa, Kentucky, Louisiana, Massachusetts, Michigan, Minnesota, Montana, Nebraska, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oregon, Pennsylvania, Rhode Island, South Dakota, Vermont, Washington, and Wisconsin), six states that were in the ‘Gained CB category’ (California, Colorado, Kansas, South Carolina, Virginia, and Wyoming), and one state in ‘Lost CB category’ (Hawaii). We excluded Alabama, Delaware, and Tennessee from the study as they did not offer emergency or comprehensive benefits over the study period.


 
