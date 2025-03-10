**** Codes for "Intrinsic Pigeon-pair Preferences Revealed by IVF" (Part 3)
*     by C. Y. Cyrus Chu, Chang-Ching Lin, Shih-Ting Lu and Ching-Yuan Kao (2024)
*
***** If you have any questions, please contact us at (cychu-4@econ.sinica.edu.tw).

****
* The program is divided into six parts:
*
* Part 1: Compilation of raw data (in STATA).
* Part 2: Estimation of the proportion of delivering mothers who adopt IVF in the mother group indexed by age (a) and family wealth-rank (w) in period (t) using Nonlinear Least Squares (NLS) regression (in STATA).
* Part 3: Estimation of the gender preference parameters of first-birth mothers who adopt IVFs (using Maximum Likelihood Estimation, MLE, in R).
* Part 4: Calculation of the MLE standard errors of parameters and the values used in Figures 3 and 4 based on the estimates obtained in Part 3 (in STATA).
* Part 5: To examine an alternative classification scheme, where wealth and age are divided into seven and five groups, respectively..
* Part 6: To employ a logit regression to examine the effects of the IVF percentage, wealth, and the IVF success rate on multiple birth outcomes and the one-boy-one-girl twin rate. 
*
*****

*****
*
* Part 6: To employ a logit regression to examine the effects of the IVF percentage, wealth, and the IVF success rate on multiple birth outcomes and the one-boy-one-girl twin rate. 
* using logit regression (in STATA).
* STATA version 15.0
*
*****

clear
set more off
cd "D:\User_data\Downloads"
use "IVF_data_03.dta", clear

gen age_group = .
replace age_group = 1 if born_mom_age >= 16 & born_mom_age <= 29
replace age_group = 2 if born_mom_age >= 30 & born_mom_age <= 32
replace age_group = 3 if born_mom_age >= 33 & born_mom_age <= 34
replace age_group = 4 if born_mom_age >= 35 & born_mom_age <= 36
replace age_group = 5 if born_mom_age >= 37 & born_mom_age <= 50

gen year_diff = .
replace year_diff = 1 if born_yr >=97 & born_yr <= 99
replace year_diff = 2 if born_yr >=100 & born_yr <= 102
replace year_diff = 3 if born_yr >=103 & born_yr <= 105
replace year_diff = 4 if born_yr >=106 & born_yr <= 108

gen lg_w=log(born_fm_wealth)
merge m:1 age_group born_yr using "ivf_at_by_year.dta",keepus(ivf_at)

*generalize the succeseful rate
gen R_t=0.299 if born_yr==97
gen R_t=0.291 if born_yr==98
gen R_t=0.294 if born_yr==99
gen R_t=0.314 if born_yr==100
gen R_t=0.311 if born_yr==101
gen R_t=0.305 if born_yr==102
gen R_t=0.290 if born_yr==103
gen R_t=0.277 if born_yr==104
gen R_t=0.280 if born_yr==105
gen R_t=0.272 if born_yr==106
gen R_t=0.285 if born_yr==107
gen R_t=0.276 if born_yr==108

*generalize the time dummy variables
gen is_yr97=1 if born_yr==97
replace is_yr97=0 if born_yr!=97

gen is_yr98=1 if born_yr==98
replace is_yr98=0 if born_yr!=98

gen is_yr99=1 if born_yr==99
replace is_yr99=0 if born_yr!=99

gen is_yr100=1 if born_yr==100
replace is_yr100=0 if born_yr!=100

gen is_yr101=1 if born_yr==101
replace is_yr101=0 if born_yr!=101

gen is_yr102=1 if born_yr==102
replace is_yr102=0 if born_yr!=102

gen is_yr103=1 if born_yr==103
replace is_yr103=0 if born_yr!=103

gen is_yr104=1 if born_yr==104
replace is_yr104=0 if born_yr!=104

gen is_yr105=1 if born_yr==105
replace is_yr105=0 if born_yr!=105

gen is_yr106=1 if born_yr==106
replace is_yr106=0 if born_yr!=106

gen is_yr107=1 if born_yr==107
replace is_yr107=0 if born_yr!=107

gen is_yr108=1 if born_yr==108
replace is_yr108=0 if born_yr!=108

*the effects of the IVF percentage, wealth, and the IVF success rate on multiple birth outcomes
logit d_twins ivf_at lg_w R_t is_yr97 is_yr98 is_yr99 is_yr100 is_yr101 is_yr102 is_yr103 is_yr104 is_yr105 is_yr106 is_yr107


*the effects of the IVF percentage, wealth, and the IVF success rate on one-boy-one-girl twin rate
drop if gender_iden==1
gen is_11=0
replace is_11=1 if gender_iden==4

logit is_11 ivf_at lg_w R_t is_yr97 is_yr98 is_yr99 is_yr100 is_yr101 is_yr102 is_yr103 is_yr104 is_yr105 is_yr106 is_yr107




*We analyze the lagged effects of these variables in period t-1.
use "IVF_data_03.dta", clear

gen age_group = .
replace age_group = 1 if born_mom_age >= 16 & born_mom_age <= 29
replace age_group = 2 if born_mom_age >= 30 & born_mom_age <= 32
replace age_group = 3 if born_mom_age >= 33 & born_mom_age <= 34
replace age_group = 4 if born_mom_age >= 35 & born_mom_age <= 36
replace age_group = 5 if born_mom_age >= 37 & born_mom_age <= 50

gen year_diff = .
replace year_diff = 1 if born_yr >=97 & born_yr <= 99
replace year_diff = 2 if born_yr >=100 & born_yr <= 102
replace year_diff = 3 if born_yr >=103 & born_yr <= 105
replace year_diff = 4 if born_yr >=106 & born_yr <= 108

gen lg_w_before=log(born_fm_wealth_before)
merge m:1 age_group born_yr using "ivf_at_by_year.dta",keepus(ivf_at_before)

*generalize the succeseful rate
gen R_t_before=0.297 if born_yr==97
gen R_t_before=0.299 if born_yr==98
gen R_t_before=0.291 if born_yr==99
gen R_t_before=0.294 if born_yr==100
gen R_t_before=0.314 if born_yr==101
gen R_t_before=0.311 if born_yr==102
gen R_t_before=0.305 if born_yr==103
gen R_t_before=0.290 if born_yr==104
gen R_t_before=0.277 if born_yr==105
gen R_t_before=0.280 if born_yr==106
gen R_t_before=0.272 if born_yr==107
gen R_t_before=0.285 if born_yr==108


*generalize the time dummy variables
gen is_yr97=1 if born_yr==97
replace is_yr97=0 if born_yr!=97

gen is_yr98=1 if born_yr==98
replace is_yr98=0 if born_yr!=98

gen is_yr99=1 if born_yr==99
replace is_yr99=0 if born_yr!=99

gen is_yr100=1 if born_yr==100
replace is_yr100=0 if born_yr!=100

gen is_yr101=1 if born_yr==101
replace is_yr101=0 if born_yr!=101

gen is_yr102=1 if born_yr==102
replace is_yr102=0 if born_yr!=102

gen is_yr103=1 if born_yr==103
replace is_yr103=0 if born_yr!=103

gen is_yr104=1 if born_yr==104
replace is_yr104=0 if born_yr!=104

gen is_yr105=1 if born_yr==105
replace is_yr105=0 if born_yr!=105

gen is_yr106=1 if born_yr==106
replace is_yr106=0 if born_yr!=106

gen is_yr107=1 if born_yr==107
replace is_yr107=0 if born_yr!=107

gen is_yr108=1 if born_yr==108
replace is_yr108=0 if born_yr!=108


logit d_twins ivf_at_before lg_w_before R_t_before is_yr97 is_yr98 is_yr99 is_yr100 is_yr101 is_yr102 is_yr103 is_yr104 is_yr105 is_yr106 is_yr107


drop if gender_iden==1
gen is_11=0
replace is_11=1 if gender_iden==4

logit is_11 ivf_at_before lg_w_before R_t_before is_yr97 is_yr98 is_yr99 is_yr100 is_yr101 is_yr102 is_yr103 is_yr104 is_yr105 is_yr106 is_yr107