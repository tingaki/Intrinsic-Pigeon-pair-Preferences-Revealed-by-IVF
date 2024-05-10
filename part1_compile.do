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
* Part 1: Compilation of raw data (in STATA).
*           STATA version 15.0
*
*****

set more off

*** Step 1: Data Cleaning 
// Step 1-1. Identification of twins and mothers' ages

use "E:\rawdata\jhq\jhq302_109.dta",replace 
// Load the raw individual data in the Ministry of Finance (MOF), with anonymous personal identification numbers (idn). 
// Fathers' idn and mothers' idn can help us in identifying babies' birthdays and family members, as well as merging 
//   their wealth and income data, and other household inforamtion.

bysort idn : drop if _N != 1
save "E:\rawdata\jhq\jhq302_109_unique.dta",replace
drop if mo_idn == "9999999999"
drop if mo_idn == ""
// Here we exclude cases transferred to the same personal identification number and those where mothers cannot be identified.

bysort mo_idn : gen siblings = _N
bysort mo_idn born_date: gen siblings_sameday = _N
gen d_twins = 0
replace d_twins = 1 if siblings_sameday == 2 
replace d_twins = 1 if siblings_sameday == 3 
// Identify siblings born on the same day as twins (Later, we will also identify siblings born one day before or after as twins)
// Note: During our study period from 2008 to 2019, there were only three instances of mothers having a four-infant delivery

rename born_date born_date_kid
gen born_yr = substr(born_date_kid,1,3)
gen born_m = substr(born_date_kid,4,2)
gen born_d = substr(born_date_kid,6,2)

destring born_yr, replace
destring born_m, replace
destring born_d, replace

gen b_transn = . 
replace b_transn = born_d if bm_n == 1 & mod(born_yr,4) == 1
replace b_transn = born_d+31 if bm_n == 2 & mod(born_yr,4) == 1 
replace b_transn = born_d+60 if bm_n == 3 & mod(born_yr,4) == 1 
replace b_transn = born_d+91 if bm_n == 4 & mod(born_yr,4) == 1 
replace b_transn = born_d+121 if bm_n == 5 & mod(born_yr,4) == 1 
replace b_transn = born_d+152 if bm_n == 6 & mod(born_yr,4) == 1
replace b_transn = born_d+182 if bm_n == 7 & mod(born_yr,4) == 1
replace b_transn = born_d+213 if bm_n == 8 & mod(born_yr,4) == 1 
replace b_transn = born_d+244 if bm_n == 9 & mod(born_yr,4) == 1 
replace b_transn = born_d+274 if bm_n == 10 & mod(born_yr,4) == 1 
replace b_transn = born_d+305 if bm_n == 11 & mod(born_yr,4) == 1 
replace b_transn = born_d+335 if bm_n == 12 & mod(born_yr,4) == 1 

replace b_transn = born_d if bm_n == 1 & mod(born_yr,4) != 1
replace b_transn = born_d+31 if bm_n == 2 & mod(born_yr,4) != 1
replace b_transn = born_d+59 if bm_n == 3 & mod(born_yr,4) != 1 
replace b_transn = born_d+90 if bm_n == 4 & mod(born_yr,4) != 1 
replace b_transn = born_d+120 if bm_n == 5 & mod(born_yr,4) != 1 
replace b_transn = born_d+151 if bm_n == 6 & mod(born_yr,4) != 1 
replace b_transn = born_d+181 if bm_n == 7 & mod(born_yr,4) != 1
replace b_transn = born_d+212 if bm_n == 8 & mod(born_yr,4) != 1 
replace b_transn = born_d+243 if bm_n == 9 & mod(born_yr,4) != 1 
replace b_transn = born_d+273 if bm_n == 10 & mod(born_yr,4) != 1 
replace b_transn = born_d+304 if bm_n == 11 & mod(born_yr,4) != 1
replace b_transn = born_d+334 if bm_n == 12 & mod(born_yr,4) != 1 
// Establish a sequence of birthdays and account for an additional day in February every four years

bysort mo_idn born_yr: gen siblings_sameyear = _N 
bysort mo_idn born_date_kid: gen siblings_sameyear_seq = _n 
// Count the siblings delivered in the same year and establish a sequence for them

gen b_transn_diffone = b_transn
replace b_transn_diffone = b_transn +1 if siblings_sameyear == 2 & siblings_sameyear_seq == 1
bysort mo_idn born_yr b_transn_diffone : gen siblings_sameday_diffone_def = _N
gen d_twins_sepc_iden = .
replace d_twins_sepc_iden = 1 if siblings_sameday_diffone_def == 2  
replace d_twins = 1 if siblings_sameday_diffone_def == 2  
// Identify siblings born one day before or after as twins
**// Note: Here, we did not consider the scenario where a mother may give birth to twins or multiple births more than once in the same year.
**// Additionally, we verified this issue using precise code definitions. Fortunately, there were no such cases identified between 2008 and 2019, nor during the base period of 1989-1991.

rename idn pers_idn 
rename mo_idn idn
merge m:1 idn using "E:\rawdata\jhq\jhq302_109_unique.dta", keepus(born_date)
drop if _merge == 2
drop _merge
rename born_date born_date_mom
gen born_yr_mom = substr(born_date_mom,1,3)
destring born_yr_mom,replace
gen born_mom_age = born_yr - born_yr_mom
rename idn mo_idn
rename pers_idn idn
// Calculate the age of the mothers at the time of giving birth

save "E:data\all_twins.dta",replace 

// Step 1-2. Identify the children who are the first-born in their family

use "E:\data\all_twins.dta",replace 
keep if born_mom_age >= 16 & born_mom_age <= 50
bysort mo_idn: gen fertility = _N
sort mo_idn born_date_kid
by mo_idn: gen fer_seq = _n

gen first_twins = 0
replace first_twins = 1 if d_twins == 1 & fer_seq  == 1 
// Generate a dummy variable indicating whether the individual is the first-born twin in a family

by mo_idn born_yr, sort: egen family_first_twins = mean(first_twins)
replace family_first_twins = 1 if family_first_twins != 0
// Generate a dummy variable indicating whether the first-born children in a family are twins

gen first_born_97108 = 0
replace first_born_97108 = 1 if fer_seq == 1 & born_yr >= 97 & born_yr <=108
by mo_idn, sort: egen family_first_97108 = mean(first_born_97108)
replace family_first_97108 = 1 if family_first_97108 != 0
// Generate a dummy variable indicating whether the first-born child in a family was born during the years 2008-2019
keep if family_first_97108 == 1
// Keep only the first-born children born during 2008-2019

gen gender_iden = .
replace gender_iden = 1 if d_twins == 0 & first_born_97108 == 1
// Identify singles: gender_iden = 1 if single-delivery

sort mo_idn d_twins 
destring idn_2,replace
by mo_idn d_twins :egen diff_gender = mean(idn_2) 
sort mo_idn d_twins 
by mo_idn d_twins :gen k = _N if d_twins == 1
replace diff_gender = 9 if k >=3 & k !=.
drop k 

replace gender_iden = 2 if d_twins == 1 & first_born_97108 == 1 & diff_gender == 1
replace gender_iden = 3 if d_twins == 1 & first_born_97108 == 1 & diff_gender == 2
replace gender_iden = 4 if d_twins == 1 & first_born_97108 == 1 & diff_gender == 1.5
replace gender_iden = 5 if diff_gender == 9
// Identify twins and multiple births:  gender_iden = 2: a two-boy-delivery, 3: a two-gril-delivery, 4: a one-boy-and-one-girl-delivery, 5: three-kids-delivery


by mo_idn d_twins born_yr, sort :gen r = _N if gender_iden == 5 
replace r = . if r != 2
replace gender_iden = 7 if gender_iden == 5 & r == 2
by mo_idn d_twins r born_yr, sort :egen diff_gender_spe = mean(idn_2) 
replace diff_gender_spe = . if r == .
replace gender_iden = 2 if d_twins == 1 & first_born_97108 == 1 & diff_gender_spe == 1 & gender_iden == 7
replace gender_iden = 3 if d_twins == 1 & first_born_97108 == 1 & diff_gender_spe == 2 & gender_iden == 7
replace gender_iden = 4 if d_twins == 1 & first_born_97108 == 1 & diff_gender_spe == 1.5 & gender_iden == 7
// Given that a mother might deliver twins more than once during her lifetime, we double-check whether there are instances where two deliveries of twins are treated as multi-births in our data

drop if first_born_97108 != 1
drop if fr_idn == "" | fr_idn == "9999999999" 
save "E:\data\obs_20082019_first.dta" ,replace

// Step 1-3. Indetify the second-born kids in a family
use "E:\data\all_twins.dta",replace 
drop if mo_idn == "" 
keep if born_mom_age >= 16 & born_mom_age <= 50
bysort mo_idn: gen fertility = _N
sort mo_idn born_date_kid
by mo_idn: gen fer_seq = _n

gen first_twins = 0
replace first_twins = 1 if d_twins == 1 & fer_seq  == 1 

by mo_idn born_yr, sort: egen family_first_twins = mean(first_twins)
replace family_first_twins = 1 if family_first_twins != 0
// Generate a dummy variable indicating that the first-born children in this family are twins, and drop the children from these families from the observations  

gen second_born_97108 = 0
replace second_born_97108 = 1 if fer_seq == 2 & born_yr >= 97 & born_yr <= 108 & family_first_twins != 1
by mo_idn, sort: egen family_second_97108 = mean(second_born_97108)
replace family_second_97108 = 1 if family_second_97108 != 0
keep if family_second_97108 == 1
// Identify the families whose second-born children were born during 2008-2019

gen second_twins = 0
replace second_twins = 1 if d_twins == 1 & fer_seq  == 2 
by mo_idn born_yr, sort: egen family_second_twins = mean(second_twins)
replace family_second_twins = 1 if family_second_twins != 0
// Identify the families whose second-born children were born during 2008-2019 and were twins at birth

destring idn_2, replace
by mo_idn  family_second_twins d_twins, sort:egen twins_gender = mean(idn_2) 
replace twins_gender = 0 if  d_twins != 1 | fer_seq != 2 | family_second_twins == 0

sort mo_idn born_yr d_twins fer_seq
by mo_idn born_yr d_twins: gen d_notsecond_twins = sum(d_twins)
sort mo_idn born_yr d_twins fer_seq
by mo_idn born_yr d_twins: egen d_count = sum(d_twins)
gen shock = 0
replace shock = 9999 if d_notsecond_twins == 1 & fer_seq != 2
by mo_idn born_yr d_twins: egen key  = sum(shock)
drop if key == 9999
gen check = .
replace check = 1 if d_notsecond_twins == 1 & fer_seq == 2
// Given that a mother might deliver twins more than once during her lifetime, we double-check whether there are instances where two deliveries of twins are treated as multi-births in our data

by mo_idn d_twins d_count, sort : egen gender_twins = sum(idn_2)
replace gender_twins = 0 if d_twins == 0 

gen twins_all_type = .
replace twins_all_type = 2 if d_twins == 1 & d_count == 2 & gender_twins == 2
replace twins_all_type = 3 if d_twins == 1 & d_count == 2 & gender_twins == 4
replace twins_all_type = 4 if d_twins == 1 & d_count == 2 & gender_twins == 3
replace twins_all_type = 5 if d_twins == 1 & d_count >= 3 
// Check the gender-composition if the second-births are twins

drop if fer_seq >= 3
// Drop those who are not the first or second birth in the family

replace con_gender = 1 if sum_gender == 1 & idn_2 == 1 & fer_seq == 2 &  family_second_twins == 0 
replace con_gender = 2 if sum_gender == 1 & idn_2 == 2 & fer_seq == 2 &  family_second_twins == 0 
replace con_gender = 3 if sum_gender == 2 & idn_2 == 1 & fer_seq == 2 &  family_second_twins == 0 
replace con_gender = 4 if sum_gender == 2 & idn_2 == 2 & fer_seq == 2 &  family_second_twins == 0 
replace con_gender = 5 if twins_all_type == 2 & sum_gender == 1
replace con_gender = 6 if twins_all_type == 3 & sum_gender == 1
replace con_gender = 7 if twins_all_type == 4 & sum_gender == 1
replace con_gender = 8 if twins_all_type == 5 & sum_gender == 1
replace con_gender = 9 if twins_all_type == 2 & sum_gender == 2
replace con_gender = 10 if twins_all_type == 3 & sum_gender == 2
replace con_gender = 11 if twins_all_type == 4 & sum_gender == 2
replace con_gender = 12 if twins_all_type == 5 & sum_gender == 2
//The gender of second-born kids is labelled as "condition gender", where 
// 1: a one-boy-delivery after a first-boy-birth;
// 2: a one-girl-delivery after a first-boy-birth;
// 3: a one-boy-delivery after a first-girl-birth;
// 4: a one-girl-delivery after a first-girl-birth;
// 5: a two-boy-delivery after a first-boy-birth;
// 6: a two-girl-delivery after a first-boy-birth;
// 7: a one-boy-and-one-girl-delivery after a first-boy-birth;
// 8: a multi-birth-delivery after a first-boy-birth;
// 9: a two-boy-delivery after a first-girl-birth;
// 10: a two-girl-delivery after a first-girl-birth;
// 11: a one-boy-and-one-girl-delivery after a first-girl-birth;
// 12: a multi-birth-delivery after a first-girl-birth.

drop if con_gender  == .
drop if second_born_97108 != 1
drop if fr_idn == "" | fr_idn == "9999999999" 
save "E:\data\obs_20082019_second.dta" ,replace 

// Step 1-4. Append the data for first-born and second-born children, and then rank the families based on their wealth

use "E:\data\obs_20082019_first.dta" ,replace
append using "E:\Ting\data\obs_20082019_second.dta"
// Append the data for first-born and second-born children during the study period from 2008 to 2019
//Here, we select mothers who gave the first births and the second births with a single birth for their first births,
// because those who had twins for their first births or already delivered births twice are unlikely to adopt IVF for their third births.
tab d_twins if born_yr >= 97 & born_yr <= 108
replace gender_iden = 1 if con_gender == 1 & fer_seq == 2
replace gender_iden = 1 if con_gender == 2 & fer_seq == 2
replace gender_iden = 1 if con_gender == 3 & fer_seq == 2
replace gender_iden = 1 if con_gender == 4 & fer_seq == 2
replace gender_iden = 2 if con_gender == 5 & fer_seq == 2
replace gender_iden = 3 if con_gender == 6 & fer_seq == 2
replace gender_iden = 4 if con_gender == 7 & fer_seq == 2
replace gender_iden = 5 if con_gender == 8 & fer_seq == 2
replace gender_iden = 2 if con_gender == 9 & fer_seq == 2
replace gender_iden = 3 if con_gender == 10 & fer_seq == 2
replace gender_iden = 4 if con_gender == 11 & fer_seq == 2
replace gender_iden = 5 if con_gender == 12 & fer_seq == 2
// Use the same gender composition label, as conditional conception is not required for this research

forvalues i = 97/108{
  local j = `i' - 1
  merge 1:1 idn using "E:\data\wealthv2\fm_wealth_`j'.dta",keepus(born_fm_wealth_`j')
  // Family wealth is calculated based on the value of an individual's house, land, stock, car, bond, and savings. Using the household data obtained in Step 1-1, we identify the children's parents and calculate their family wealth
  drop if _merge == 2
  drop _merge
  replace born_fm_wealth_`j' = . if born_yr != `i'
  drop if  born_fm_wealth_`j' <= 3000 & born_yr == `i'
  //Given that families with no or very low wealth are unlikely to adopt IVF, we restricted our analysis to mothers whose combined wealth with their spouse (or as single mothers) exceeded NT$3,000 (USD $100).
  xtile pr_born_fm_wealth_`j' = born_fm_wealth_`j',nq(10)
 }
 
gen pr_born_yr_before = .
forvalues i = 96/107{
  local j = `i' +1
  replace pr_born_yr_before = pr_born_fm_wealth_`i' if born_yr ==`j'
 }
//  Merge using the family wealth of the children and convert it to wealth rank with 10 quantiles. The wealth rank is calculated by comparing the total family wealth to those who also had deliveries in the same year
save "E:\data\obs_20082019_wealth.dta", replace

// Step 1-5. Calculate the values for Figure 1

use "E:\data\obs_20082019_wealth.dta", replace
forvalues i = 1/10{
 tab d_twins if wr_group == `i'
 }

use "E:\data\obs_19891991_wealth.dta", replace 
forvalues i = 1/10{
 tab d_twins if wr_group == `i'
 }

// Step 2. Define variables for Part 2 

use "E:\data\obs_20082019_wealth.dta", replace
gen age_group = .
replace age_group = 1 if born_mom_age >= 16 & born_mom_age <= 29
replace age_group = 2 if born_mom_age >= 30 & born_mom_age <= 32
replace age_group = 3 if born_mom_age >= 33 & born_mom_age <= 34
replace age_group = 4 if born_mom_age >= 35 & born_mom_age <= 36
replace age_group = 5 if born_mom_age >= 37 & born_mom_age <= 50
// Divide observations into 5 age groups 

replace year_diff = 1 if born_yr >=97 & born_yr <= 99
replace year_diff = 2 if born_yr >=100 & born_yr <= 102
replace year_diff = 3 if born_yr >=103 & born_yr <= 105
replace year_diff = 4 if born_yr >=106 & born_yr <= 108
// Divide observations into 4 time periods
 
gen k = .
gen k_all = .
gen k_portion = .
forvalues p = 0/1{
forvalues i = 1/10{
 forvalues j = 1/5{
 	forvalues m = 1/4{
     forvalues k = 1/5{
	  count if wr_group ==`i' & age_group ==`j' & gender_iden ==`k' & year_diff == `m' & d_twins == `p'
      replace k_portion = r(N) if wr_group ==`i' & age_group ==`j' & gender_iden ==`k' & year_diff == `m' & d_twins == `p'
	  }
    }
  }
 }
}
// Count the observations in each gender-composition, age, wealth, year, and twins_dummy group 

preserve
use "E:\data\obs_19891991_wealth.dta", replace
//The data from 1989-1991, which has been merged and compiled through steps 1-2 to 1-4 by replace the study period 2008-2019 with 1989-1991, contains the same columns as "E:\Ting\data\obs_20082019_wealth.dta"
forvalues i = 1/5{
 tab d_twins if age_group == `i'
 }
restore
 
gen tr_nature = . 
replace tr_nature = 0.0076 if age_group == 1
replace tr_nature = 0.0098 if age_group == 2
replace tr_nature = 0.0111 if age_group == 3
replace tr_nature = 0.0108 if age_group == 4
replace tr_nature = 0.0080 if age_group == 5
// Generate the variable representing the natural twin birth rate, C_a, using the data from 1989-1991

gen at_k_portion = .
 forvalues j = 1/5{
 	forvalues m = 1/4{
	  count if age_group ==`j' & year_diff == `m' 
      replace at_k_portion = r(N) if age_group ==`j' & year_diff == `m' 
	  }
    }

gen ivf_adopt_at = .
replace ivf_adopt_at = 1253 if age_group == 1 & year_diff == 1
replace ivf_adopt_at = 1442 if age_group == 1 & year_diff == 2
replace ivf_adopt_at = 1936 if age_group == 1 & year_diff == 3
replace ivf_adopt_at = 1883 if age_group == 1 & year_diff == 4
 
replace ivf_adopt_at = 1941 if age_group == 2 & year_diff == 1
replace ivf_adopt_at = 2948 if age_group == 2 & year_diff == 2
replace ivf_adopt_at = 3743 if age_group == 2 & year_diff == 3
replace ivf_adopt_at = 3529 if age_group == 2 & year_diff == 4
 
replace ivf_adopt_at = 1590 if age_group == 3 & year_diff == 1
replace ivf_adopt_at = 2654 if age_group == 3 & year_diff == 2
replace ivf_adopt_at = 3795 if age_group == 3 & year_diff == 3
replace ivf_adopt_at = 3652 if age_group == 3 & year_diff == 4

replace ivf_adopt_at = 1329 if age_group == 4 & year_diff == 1
replace ivf_adopt_at = 2478 if age_group == 4 & year_diff == 2
replace ivf_adopt_at = 3602 if age_group == 4 & year_diff == 3
replace ivf_adopt_at = 4090 if age_group == 4 & year_diff == 4

replace ivf_adopt_at = 1587 if age_group == 5 & year_diff == 1
replace ivf_adopt_at = 3197 if age_group == 5 & year_diff == 2
replace ivf_adopt_at = 4754 if age_group == 5 & year_diff == 3
replace ivf_adopt_at = 7195 if age_group == 5 & year_diff == 4
// ivf_adopt_at represents the total number of IVF adopters whose treatments are successful in the a-t group, sourced from the public website of the Health Promotion Administration, Ministry of Health and Welfare, Taiwan


gen ivf_at = ivf_adopt_at/at_k_portion
// Generate the variable ivf_at 
sort age_group wr_group gender_iden year_diff d_twins
by age_group wr_group gender_iden year_diff d_twins:gen iu = _n
drop if iu != 1
keep age_group wr_group gender_iden year_diff k_portion tr_nature ivf_at d_twins
reshape wide k_portion, i(age_group wr_group year_diff d_twins) j(gender_iden)
drop if wr_group == . 

replace k_portion1 = 0 if k_portion1 == .
replace k_portion2 = 0 if k_portion2 == .
replace k_portion3 = 0 if k_portion3 == .
replace k_portion4 = 0 if k_portion4 == .
replace k_portion5 = 0 if k_portion5 == .
rename k_portion1 var1
rename k_portion2 var2
rename k_portion3 var3
rename k_portion4 var4
rename k_portion5 var5
// Reshape the data from birth-unit to a total of 200 groups indexed by a, w, and t. Var1-5 represent the number of each type of delivery in each a-w-t group
//  var1: # of singles
//  var2: # of two-boy deliveries
//  var3: # of two-girl deliveries
//  var4: # of one-boy-and-one-girl deliveries
//  var5: # of multi-birth deliveries

gen y = d_twins

forvalues i = 1/10{
  gen dummy_`i' = 0
  replace dummy_`i' = 1 if wr_group  == `i'
  }
  
forvalues i = 1/5{
  gen dummyage_`i' = 0
  replace dummyage_`i' = 1 if age_group  == `i'
  }
 
forvalues i = 1/10{
  gen d`i' = dummy_`i'* ivf_at 
  }
   
forvalues i = 1/5{
  forvalues j = 1/10{
  gen e`i'`j' = dummyage_`i' * dummy_`j'
  } 
 }
// Creat dummy variables for age x wealth groups

gen all_popu = var1 + var2 + var3 + var4 + var5 
drop var1 var2 var3 var4 var5
save "D:\User_data\Downloads\IVF_data_01.dta", replace

**** End of Code ****
