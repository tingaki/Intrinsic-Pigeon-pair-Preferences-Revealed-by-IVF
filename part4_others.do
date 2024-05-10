**** Codes for "Intrinsic Pigeon-pair Preferences Revealed by IVF" (Part 3)
*     by C. Y. Cyrus Chu, Chang-Ching Lin, and Shih-Ting Lu (2024)
*
***** If you have any questions, please contact us at (cychu-4@econ.sinica.edu.tw).

****
* The program is divided into four parts:
*
* Part 1: Compilation of raw data (in STATA).
* Part 2: Estimation of the proportion of delivering mothers who adopt IVF in the mother group indexed by age (a) and family wealth-rank (w) in period (t) using Nonlinear Least Squares (NLS) regression (in STATA).
* Part 3: Estimation of the gender preference parameters of first-birth mothers who adopt IVFs (using Maximum Likelihood Estimation, MLE, in R).
* Part 4: Calculation of the MLE standard errors of parameters and the values used in Figures 3(a) and (b) based on the estimates obtained in Part 3 (in STATA). 
*
*****

*****
*
* Part 4: Calculation of the MLE standard errors of parameters and the values 
*            used in Figures 3(a) and (b) based on the estimates obtained in Part 3 
*            (in STATA). 
*           STATA version 15.0
*
*****

clear
set more off
cd "D:\User_data\Downloads"

use "IVF_data_02.dta", replace

// Calculate the values for Figure 3 (a)
// (one-boy-and-one-girl)
gen total_twins =  var2+var3+var4+var5
bysort wr_group year_diff:egen total =sum (total_twins)

gen pred_11 = .
replace pred_11 = 0.2042*(1-0.9417671*ivf_twins)+0.533186*0.9417671*ivf_twins if year_diff == 1
replace pred_11 = 0.2042*(1-0.9110994*ivf_twins)+0.533186*0.9110994*ivf_twins if year_diff == 2
replace pred_11 = 0.2042*(1-0.696733*ivf_twins)+0.533186*0.696733*ivf_twins if year_diff == 3
replace pred_11 = 0.2042*(1-0.6965398*ivf_twins)+0.533186*0.6965398*ivf_twins if year_diff == 4
// Accroding Equation (4), we calculate the predicted porportions by using estimated values of theta_j and gamma_t from Part 3 

gen k = pred_11* total_twins/total
bysort wr_group year_diff:egen predicted_p11 =sum (k)
// Compute the weighted average of delta_w4's across age groups, using the number of multiple births in each age group as weights.

gen theo_11 = .
replace theo_11 = 0.2042*(1-0.9417671*ivf_twins)+0.4969777*0.9417671*ivf_twins if year_diff == 1
replace theo_11 = 0.2042*(1-0.9110994*ivf_twins)+0.4969777*0.9110994*ivf_twins if year_diff == 2
replace theo_11 = 0.2042*(1-0.696733*ivf_twins)+0.4969777*0.696733*ivf_twins if year_diff == 3
replace theo_11 = 0.2042*(1-0.6965398*ivf_twins)+0.4969777*0.6965398*ivf_twins if year_diff == 4
// Accroding Equation (4), we alculate the thereotical porportion based on the estimated gender_neutral rates, and the estimated values of gamma_t's from Part 3

gen kk = theo_11* total_twins/total
bysort wr_group year_diff:egen theo_p11 =sum (kk)
// Compute the weighted average of the gender_neutral delta_w4's across age groups, using the number of multiple births in each age group as weights.

preseve
use "E:\data\obs_19891991_wealth.dta", replace
//The data from 1989-1991, which has been merged and compiled through steps 1-2 to 1-4 in part 1 by replace the study period 2008-2019 with 1989-1991, contains the same columns as "E:\Ting\data\obs_20082019_wealth.dta"
keep if gender_iden != 1
gen oneone_in_w = .
gen all_multi_in_w = .
gen p11_in_w_t0 = .
forvalues i = 1/10{
 count if gender_iden == 4 &wr_group == `i'
 replace oneone_in_w = r_(N) if gender_iden == 4 & wr_group == `i'
 count if wr_group == `i'
 replace all_multi_in_w = r_(N) if wr_group == `i'
 replace p11_in_w_t0 = oneone_in_w /all_multi_in_w
 duplicates drop wr_group,force
 }
save "E:\data\obs_19891991_wealth_p11.dta", replace
//We calculate one-boy-and-one-girl twin proportions across wealth rankings in 1989-1991, which are used as actual proportions during the baseline period
restore

merge m:1 wr_group using "E:\data\obs_19891991_wealth_p11.dta", keepus(p11_in_w_t0)
drop _merge 
rename wr_group wealth_group
order wealth_group p11_in_w_t0 predicted_p11 theo_p11

// Calculate the values for Figure S2 in Supplimentary Material
// (two-girl)
use "IVF_data_02.dta", replace
gen total_twins =  var2+var3+var4+var5
bysort wr_group year_diff:egen o_two =sum (var3)
bysort wr_group year_diff:egen total =sum (total_twins)
gen real_p02 = o_two/total
gen pred_02 = .
replace pred_02 = 0.3912*(1-0.9417671*ivf_twins)+0.2179887*0.9417671*ivf_twins if year_diff == 1
replace pred_02 = 0.3912*(1-0.9110994*ivf_twins)+0.2179887*0.9110994*ivf_twins if year_diff == 2
replace pred_02 = 0.3912*(1-0.696733*ivf_twins)+0.2179887*0.696733*ivf_twins if year_diff == 3
replace pred_02 = 0.3912*(1-0.6965398*ivf_twins)+0.2179887*0.6965398*ivf_twins if year_diff == 4
// Accroding Equation (4), we calculate the predicted porportions by using estimated values of theta_j and gamma_t from Part 3 

gen k = pred_02* total_twins/total
bysort wr_group year_diff:egen predicted_p02 =sum (k)
// Compute the weighted average of delta_w4's across age groups, using the number of multiple births in each age group as weights.

gen theo_02 = .
replace theo_02 = 0.3912*(1-0.9417671*ivf_twins)+0.2328921*0.9417671*ivf_twins if year_diff == 1
replace theo_02 = 0.3912*(1-0.9110994*ivf_twins)+0.2328921*0.9110994*ivf_twins if year_diff == 2
replace theo_02 = 0.3912*(1-0.696733*ivf_twins)+0.2328921*0.696733*ivf_twins if year_diff == 3
replace theo_02 = 0.3912*(1-0.6965398*ivf_twins)+0.2328921*0.6965398*ivf_twins if year_diff == 4
// Accroding Equation (4), we alculate the thereotical porportion based on the estimated gender_neutral rates, and the estimated values of gamma_t's from Part 3

gen kk = theo_02* total_twins/total
bysort wr_group year_diff:egen theo_p02 =sum (kk)
// Compute the weighted average of the gender_neutral delta_w4's across age groups, using the number of multiple births in each age group as weights.

preseve
use "E:\data\obs_19891991_wealth.dta", replace
//The data from 1989-1991, which has been merged and compiled through steps 1-2 to 1-4 in part 1 by replace the study period 2008-2019 with 1989-1991, contains the same columns as "E:\Ting\data\obs_20082019_wealth.dta"
keep if gender_iden != 1
gen two_girl_in_w = .
gen all_multi_in_w = .
gen p02_in_w_t0 = .
forvalues i = 1/10{
 count if gender_iden == 3 &wr_group == `i'
 replace two_girl_in_w = r_(N) if gender_iden == 3 & wr_group == `i'
 count if wr_group == `i'
 replace all_multi_in_w = r_(N) if wr_group == `i'
 replace p02_in_w_t0 = two_girl_in_w /all_multi_in_w
 duplicates drop wr_group,force
 }
save "E:\data\obs_19891991_wealth_p02.dta", replace
//We calculate two-girl twin proportions across wealth rankings in 1989-1991, which are used as actual proportions during the baseline period
restore

merge m:1 wr_group using "E:\data\obs_19891991_wealth_p02.dta", keepus(p02_in_w_t0)
drop _merge 
rename wr_group wealth_group
order wealth_group p02_in_w_t0 predicted_p02 theo_p02

// Calculate the values for Figure 3 (b)
// (two-boy)
use "IVF_data_02.dta", replace
gen total_twins =  var2+var3+var4+var5
bysort wr_group year_diff:egen two_o =sum (var2)
bysort wr_group year_diff:egen total =sum (total_twins)
gen real_p20 = two_o/total
gen pred_20 = .
replace pred_20 = 0.3861*(1-0.9417671*ivf_twins)+0.243591*0.9417671*ivf_twins if year_diff == 1
replace pred_20 = 0.3861*(1-0.9110994*ivf_twins)+0.243591*0.9110994*ivf_twins if year_diff == 2
replace pred_20 = 0.3861*(1-0.696733*ivf_twins)+0.243591*0.696733*ivf_twins if year_diff == 3
replace pred_20 = 0.3861*(1-0.6965398*ivf_twins)+0.243591*0.6965398*ivf_twins if year_diff == 4
// Accroding Equation (4), we calculate the predicted porportions by using estimated values of theta_j and gamma_t from Part 3 

gen k = pred_20* total_twins/total
bysort wr_group year_diff:egen predicted_p20 =sum (k)
// Compute the weighted average of delta_w4's across age groups, using the number of multiple births in each age group as weights.

gen theo_20 = .
replace theo_20 = 0.3861*(1-0.9417671*ivf_twins)+0.2651301*0.9417671*ivf_twins if year_diff == 1
replace theo_20 = 0.3861*(1-0.9110994*ivf_twins)+0.2651301*0.9110994*ivf_twins if year_diff == 2
replace theo_20 = 0.3861*(1-0.696733*ivf_twins)+0.2651301*0.696733*ivf_twins if year_diff == 3
replace theo_20 = 0.3861*(1-0.6965398*ivf_twins)+0.2651301*0.6965398*ivf_twins if year_diff == 4
// Accroding Equation (4), we alculate the thereotical porportion based on the estimated gender_neutral rates, and the estimated values of gamma_t's from Part 3

gen kk = theo_20* total_twins/total
bysort wr_group year_diff:egen theo_p20 =sum (kk)
// Compute the weighted average of the gender_neutral delta_w4's across age groups, using the number of multiple births in each age group as weights.

preseve
use "E:\data\obs_19891991_wealth.dta", replace
//The data from 1989-1991, which has been merged and compiled through steps 1-2 to 1-4 in part 1 by replace the study period 2008-2019 with 1989-1991, contains the same columns as "E:\Ting\data\obs_20082019_wealth.dta"
keep if gender_iden != 1
gen two_boy_in_w = .
gen all_multi_in_w = .
gen p20_in_w_t0 = .
forvalues i = 1/10{
 count if gender_iden == 2 &wr_group == `i'
 replace two_boy_in_w = r_(N) if gender_iden == 2 & wr_group == `i'
 count if wr_group == `i'
 replace all_multi_in_w = r_(N) if wr_group == `i'
 replace p20_in_w_t0 = two_boy_in_w /all_multi_in_w
 duplicates drop wr_group,force
 }
save "E:\data\obs_19891991_wealth_p20.dta", replace
//We calculate two-boy twin proportions across wealth rankings in 1989-1991, which are used as actual proportions during the baseline period
restore

merge m:1 wr_group using "E:\data\obs_19891991_wealth_p20.dta", keepus(p20_in_w_t0)
drop _merge 
rename wr_group wealth_group
order wealth_group p20_in_w_t0 predicted_p20 theo_p20

// Calcualte the MLE gradient and standard errors of theta_j's and gamma_t's
use "IVF_data_02.dta", replace

gen theta20 = 0.243591
gen theta02 = 0.2179887
gen theta11 = 0.533186
gen theta3 = 1- theta20 - theta02 - theta11
gen gamma1 = 0.9417671
gen gamma2 = 0.9110994
gen gamma3 = 0.696733
gen gamma4 = 0.6965398
gen delta20 = 0.3861 //delta20_bar in equation (4)
gen delta02 = 0.3912 //delta02_bar in equation (4)
gen delta11 = 0.2042 //delta11_bar in equation (4)
gen delta3 = 0.0185 //delta3_bar in equation (4)
gen delta_awt20 = .
gen delta_awt02 = .
gen delta_awt11 = .
gen delta_awt3 = .
//generate variables represent the estimated results of theta_j and gamma_t

forvalues i = 1/7{
  gen column`i'_20 = 0
  }

forvalues i = 1/7{
  gen column`i'_02 = 0
  }

forvalues i = 1/7{
  gen column`i'_11 = 0
  }

forvalues i = 1/7{
  gen column`i'_3 = 0
  } 
// Generate four matrices representing GA, GB, GC, and GD.

forvalues j = 1/4{
  gen k`j' = gamma`j'*ivf_twins
  }
 
forvalues j = 1/4{  
  replace delta_awt20 = delta20*(1-k`j')+theta20*(k`j') if year_diff == `j'  
  }

forvalues j = 1/4{  
  replace delta_awt02 = delta02*(1-k`j')+theta02*(k`j') if year_diff == `j'  
  }
  
forvalues j = 1/4{  
  replace delta_awt11 = delta11*(1-k`j')+theta11*(k`j') if year_diff == `j'  
  }
  
forvalues j = 1/4{  
  replace delta_awt3 = delta3*(1-k`j')+theta3*(k`j') if year_diff == `j'  
  }  
 

 forvalues i = 4/7{
  local j = `i' -3
  replace column`i'_20 =(1/delta_awt20)*(theta20-delta20)*ivf_twins*sqrt(var2) if year_diff ==`j'
  }

forvalues i = 4/7{
  local j = `i' -3
  replace column`i'_02 =(1/delta_awt02)*(theta20-delta02)*ivf_twins*sqrt(var3) if year_diff ==`j'
  }

forvalues i = 4/7{
  local j = `i' -3
  replace column`i'_11 =(1/delta_awt11)*(theta11-delta11)*ivf_twins*sqrt(var4) if year_diff ==`j'
  }

forvalues i = 4/7{
  local j = `i' -3
  replace column`i'_3 =(1/delta_awt3)*(theta20-delta3)*ivf_twins*sqrt(var5) if year_diff ==`j'
  }   
// Replace the figures in each matrix by the partial derivative of MLE function to theta_j
  
  
forvalues i = 1/4{ 
  replace column1_20 = k`i'*(1/delta_awt20)*sqrt(var2) if year_diff == `i'
  }

forvalues i = 1/4{ 
  replace column2_02 = k`i'*(1/delta_awt02)*sqrt(var3) if year_diff == `i'
  }

forvalues i = 1/4{ 
  replace column3_11 = k`i'*(1/delta_awt11)*sqrt(var4) if year_diff == `i'
  }

forvalues i = 1/4{ 
  replace column1_3 = -k`i'*(1/delta_awt3)*sqrt(var5) if year_diff == `i'
  replace column2_3 = -k`i'*(1/delta_awt3)*sqrt(var5) if year_diff == `i'
  replace column3_3 = -k`i'*(1/delta_awt3)*sqrt(var5) if year_diff == `i'
  }
// Replace the figures in each matrix by the partial derivative of MLE function to gamma_t
//  column1_20 to colun7_20 represents a 200*7 matrix = GA
//  column1_02 to colun7_02 represents a 200*7 matrix = GB
//  column1_11 to colun7_11 represents a 200*7 matrix = GC
//  column1_3  to colun7_3 represents a 200*7 matrix = GD
  
// Based on the outer-product of the gradient, we calcaute the variance matrix in the form of (G'G)^(-1), such that (G'G) = GA'GA+GB'GB+GC'GC+GD'GD = H, where
matrix H = (1052320.757,1023165.234,1023165.234,-75835.79938,-72314.76635,-65282.40786,-74181.25517\1023165.234,1054611.289,1023165.234,-75877.17979,-72441.62566,-65519.38382,-74421.2062\1023165.234,1023165.234,1041990.347,-74114.0971,-69352.68069,-61518.17058,-69292.43458\-75835.79938,-75877.17979,-74114.0971,18548.98852,0,0,0\-72314.76635,-72441.62566,-69352.68069,0,18592.99774,0,0\-65282.40786,	-65519.38382,-61518.17058,0,0,	22377.79019,0\-74181.25517,-74421.2062,-72170.39962,0,0,0,24210.69274)
mat V = (inv((H)))
matrix list V

//(G'G)^(-1) = V

V[7,7]
            r1          r2          r3          r4          r5          r6          r7
c1    .0001456   .00011327  -.00011364   .00060456   .00058372   .00044399   .00046905
c2   .00011181   .00014552  -.00011238   .00060341   .00058269   .00044333   .00046829
c3   -.0001334  -.00013353   .00012585  -.00058879  -.00056969  -.00043417  -.00045902
c4   .00051963   .00052484  -.00042146   .00264134   .00249385   .00189397   .00199922
c5   .00050433   .00050946   -.0004104    .0025061   .00246941   .00183467   .00193668
c6   .00038539   .00038943  -.00031458   .00191174   .00184283   .00144439   .00147757
c7   .00039215   .00039634  -.00031848   .00195202   .00188146   .00142891   .00154964

	   
//forvalues 1 =1/7, c`i'r`i' are the variances of theta20,theta02, theta11, gamma_1, gamma_2, gamma_3 and gamma_4, respectively
