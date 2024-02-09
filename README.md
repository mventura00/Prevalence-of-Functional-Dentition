# Prevalence-of-Functional-Dentition

This is our R-code for the paper we are submitting to the CDC for clearance and then to the Journal of Dental Research. 

We used data from the Behavioral Risk Factor Surveillance Survey (BRFSS) for the years 2007 to 2018.  Note that BRFSS uses multistage sampling.  We therefore performed our analysis in R-Studio (v.4.3.0) to account for the oversampling and multistage clustering used in the survey. 

We estimate the impact of the change in dental benefits from Medicaid expansion in different US states on tooth retention in 2012 and 2018 among likely Medicaid eligible and not likely Medicaid eligible adults on adults aged 35 to 54 years (i.e., working aged adults). We use non-Medicaid eligibles as controls and apply a difference in difference model. 

We used a difference in difference in difference (DID) model through ordinal logistic regression to examine the impact of changing dental benefits from a state’s Medicaid policy on tooth retention for Medicaid eligible results (25 to 54 years of age). 

 
