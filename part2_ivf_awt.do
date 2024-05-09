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
* Part 2: Estimation of the proportion of delivering mothers who adopt IVF in 
*         the mother group indexed by age (a) and family wealth-rank (w) in period (t) 
*         using Nonlinear Least Squares (NLS) regression (in STATA).
*
*****

clear
set more off

cd "D:\User_data\Downloads"

use "IVF_data_01.dta", clear
rename wealth_r wr_group
capture drop twr
gen twr=y01
capture drop trn
gen trn=tr_nature 
*capture drop ivf
gen ivf=ivf_at
capture drop wpop
gen wpop=sub_pop
* Old Code

* Substitutable Expression Program
capture program drop nlivfawt5

program nlivfawt5, rclass
version 15.0
syntax varlist(min=53 max=53) [aw fw iw] [if]
local twr : word 1 of `varlist'
local ivf : word 2 of `varlist'
local trn : word 3 of `varlist'
local d11 : word 4 of `varlist'
local d12 : word 5 of `varlist'
local d13 : word 6 of `varlist'
local d14 : word 7 of `varlist'
local d15 : word 8 of `varlist'
local d16 : word 9 of `varlist'
local d17 : word 10 of `varlist'
local d18 : word 11 of `varlist'
local d19 : word 12 of `varlist'
local d110 : word 13 of `varlist'
local d21 : word 14 of `varlist'
local d22 : word 15 of `varlist'
local d23 : word 16 of `varlist'
local d24 : word 17 of `varlist'
local d25 : word 18 of `varlist'
local d26 : word 19 of `varlist'
local d27 : word 20 of `varlist'
local d28 : word 21 of `varlist'
local d29 : word 22 of `varlist'
local d210 : word 23 of `varlist'
local d31 : word 24 of `varlist'
local d32 : word 25 of `varlist'
local d33 : word 26 of `varlist'
local d34 : word 27 of `varlist'
local d35 : word 28 of `varlist'
local d36 : word 29 of `varlist'
local d37 : word 30 of `varlist'
local d38 : word 31 of `varlist'
local d39 : word 32 of `varlist'
local d310 : word 33 of `varlist'
local d41 : word 34 of `varlist'
local d42 : word 35 of `varlist'
local d43 : word 36 of `varlist'
local d44 : word 37 of `varlist'
local d45 : word 38 of `varlist'
local d46 : word 39 of `varlist'
local d47 : word 40 of `varlist'
local d48 : word 41 of `varlist'
local d49 : word 42 of `varlist'
local d410 : word 43 of `varlist'
local d51 : word 44 of `varlist'
local d52 : word 45 of `varlist'
local d53 : word 46 of `varlist'
local d54 : word 47 of `varlist'
local d55 : word 48 of `varlist'
local d56 : word 49 of `varlist'
local d57 : word 50 of `varlist'
local d58 : word 51 of `varlist'
local d59 : word 52 of `varlist'
local d510 : word 53 of `varlist'
local g1 "exp({b1})"
local g2 "exp({b2})"
local g3 "exp({b3})"
local g4 "exp({b4})"
local g5 "exp({b5})"
local g6 "exp({b6})"
local g7 "exp({b7})"
local g8 "exp({b8})"
local g9 "exp({b9})"
local g10 "exp({b10})"
// Here, we constrain the value of beta_w, the twins rate of IVF adopters in each group, to be positive.
local h11 "-0.0085 + 0.0172*exp({a11})/(1+exp({a11}))"
// Constrain the value of the IVF adoption rate (in group a=1, w=1, t=1) to be positive
local h12 "{a12}"
local h13 "{a13}"
local h14 "{a14}"
local h15 "{a15}"
local h16 "{a16}"
local h17 "{a17}"
local h18 "{a18}"
local h19 "{a19}"
local h21 "{a21}"
local h22 "{a22}"
local h23 "{a23}"
local h24 "{a24}"
local h25 "{a25}"
local h26 "{a26}"
local h27 "{a27}"
local h28 "{a28}"
local h29 "{a29}"
local h31 "{a31}"
local h32 "{a32}"
local h33 "{a33}"
local h34 "{a34}"
local h35 "{a35}"
local h36 "{a36}"
local h37 "{a37}"
local h38 "{a38}"
local h39 "{a39}"
local h41 "{a41}"
local h42 "{a42}"
local h43 "{a43}"
local h44 "{a44}"
local h45 "{a45}"
local h46 "{a46}"
local h47 "{a47}"
local h48 "{a48}"
local h49 "{a49}"
local h51 "{a51}"
local h52 "{a52}"
local h53 "{a53}"
local h54 "{a54}"
local h55 "{a55}"
local h56 "{a56}"
local h57 "{a57}"
local h58 "{a58}"
local h59 "{a59}"

local h110 "(3.503*`h11'+3.241*`h12'+2.741*`h13'+2.303*`h14'+1.966*`h15'+1.685*`h16'+1.453*`h17'+1.264*`h18'+1.096*`h19')"
local h210 "(1.085*`h21'+1.160*`h22'+1.249*`h23'+1.281*`h24'+1.290*`h25'+1.265*`h26'+1.229*`h27'+1.166*`h28'+1.093*`h29')"
local h310 "(0.604*`h31'+0.653*`h32'+0.768*`h33'+0.859*`h34'+0.937*`h35'+0.996*`h36'+1.027*`h37'+1.055*`h38'+1.045*`h39')"
local h410 "(0.423*`h41'+0.440*`h42'+0.525*`h43'+0.625*`h44'+0.716*`h45'+0.799*`h46'+0.874*`h47'+0.935*`h48'+0.982*`h49')"
local h510 "(0.255*`h51'+0.248*`h52'+0.313*`h53'+0.390*`h54'+0.447*`h55'+0.523*`h56'+0.608*`h57'+0.706*`h58'+0.840*`h59')"
// See footnote 3 for the above relationships.

local t11 "(1-`ivf'-`h11')*`d11'*`trn'+(`ivf'+`h11')*`d11'*`g1'"
local t12 "(1-`ivf'-`h12')*`d12'*`trn'+(`ivf'+`h12')*`d12'*`g2'"
local t13 "(1-`ivf'-`h13')*`d13'*`trn'+(`ivf'+`h13')*`d13'*`g3'"
local t14 "(1-`ivf'-`h14')*`d14'*`trn'+(`ivf'+`h14')*`d14'*`g4'"
local t15 "(1-`ivf'-`h15')*`d15'*`trn'+(`ivf'+`h15')*`d15'*`g5'"
local t16 "(1-`ivf'-`h16')*`d16'*`trn'+(`ivf'+`h16')*`d16'*`g6'"
local t17 "(1-`ivf'-`h17')*`d17'*`trn'+(`ivf'+`h17')*`d17'*`g7'"
local t18 "(1-`ivf'-`h18')*`d18'*`trn'+(`ivf'+`h18')*`d18'*`g8'"
local t19 "(1-`ivf'-`h19')*`d19'*`trn'+(`ivf'+`h19')*`d19'*`g9'"
local t20 "(1-`ivf'+`h110')*`d110'*`trn'+(`ivf'-`h110')*`d110'*`g10'"
local t21 "(1-`ivf'-`h21')*`d21'*`trn'+(`ivf'+`h21')*`d21'*`g1'"
local t22 "(1-`ivf'-`h22')*`d22'*`trn'+(`ivf'+`h22')*`d22'*`g2'"
local t23 "(1-`ivf'-`h23')*`d23'*`trn'+(`ivf'+`h23')*`d23'*`g3'"
local t24 "(1-`ivf'-`h24')*`d24'*`trn'+(`ivf'+`h24')*`d24'*`g4'"
local t25 "(1-`ivf'-`h25')*`d25'*`trn'+(`ivf'+`h25')*`d25'*`g5'"
local t26 "(1-`ivf'-`h26')*`d26'*`trn'+(`ivf'+`h26')*`d26'*`g6'"
local t27 "(1-`ivf'-`h27')*`d27'*`trn'+(`ivf'+`h27')*`d27'*`g7'"
local t28 "(1-`ivf'-`h28')*`d28'*`trn'+(`ivf'+`h28')*`d28'*`g8'"
local t29 "(1-`ivf'-`h29')*`d29'*`trn'+(`ivf'+`h29')*`d29'*`g9'"
local t30 "(1-`ivf'+`h210')*`d210'*`trn'+(`ivf'-`h210')*`d210'*`g10'"
local t31 "(1-`ivf'-`h31')*`d31'*`trn'+(`ivf'+`h31')*`d31'*`g1'"
local t32 "(1-`ivf'-`h32')*`d32'*`trn'+(`ivf'+`h32')*`d32'*`g2'"
local t33 "(1-`ivf'-`h33')*`d33'*`trn'+(`ivf'+`h33')*`d33'*`g3'"
local t34 "(1-`ivf'-`h34')*`d34'*`trn'+(`ivf'+`h34')*`d34'*`g4'"
local t35 "(1-`ivf'-`h35')*`d35'*`trn'+(`ivf'+`h35')*`d35'*`g5'"
local t36 "(1-`ivf'-`h36')*`d36'*`trn'+(`ivf'+`h36')*`d36'*`g6'"
local t37 "(1-`ivf'-`h37')*`d37'*`trn'+(`ivf'+`h37')*`d37'*`g7'"
local t38 "(1-`ivf'-`h38')*`d38'*`trn'+(`ivf'+`h38')*`d38'*`g8'"
local t39 "(1-`ivf'-`h39')*`d39'*`trn'+(`ivf'+`h39')*`d39'*`g9'"
local t40 "(1-`ivf'+`h310')*`d310'*`trn'+(`ivf'-`h310')*`d310'*`g10'"
local t41 "(1-`ivf'-`h41')*`d41'*`trn'+(`ivf'+`h41')*`d41'*`g1'"
local t42 "(1-`ivf'-`h42')*`d42'*`trn'+(`ivf'+`h42')*`d42'*`g2'"
local t43 "(1-`ivf'-`h43')*`d43'*`trn'+(`ivf'+`h43')*`d43'*`g3'"
local t44 "(1-`ivf'-`h44')*`d44'*`trn'+(`ivf'+`h44')*`d44'*`g4'"
local t45 "(1-`ivf'-`h45')*`d45'*`trn'+(`ivf'+`h45')*`d45'*`g5'"
local t46 "(1-`ivf'-`h46')*`d46'*`trn'+(`ivf'+`h46')*`d46'*`g6'"
local t47 "(1-`ivf'-`h47')*`d47'*`trn'+(`ivf'+`h47')*`d47'*`g7'"
local t48 "(1-`ivf'-`h48')*`d48'*`trn'+(`ivf'+`h48')*`d48'*`g8'"
local t49 "(1-`ivf'-`h49')*`d49'*`trn'+(`ivf'+`h49')*`d49'*`g9'"
local t50 "(1-`ivf'+`h410')*`d410'*`trn'+(`ivf'-`h410')*`d410'*`g10'"
local t51 "(1-`ivf'-`h51')*`d51'*`trn'+(`ivf'+`h51')*`d51'*`g1'"
local t52 "(1-`ivf'-`h52')*`d52'*`trn'+(`ivf'+`h52')*`d52'*`g2'"
local t53 "(1-`ivf'-`h53')*`d53'*`trn'+(`ivf'+`h53')*`d53'*`g3'"
local t54 "(1-`ivf'-`h54')*`d54'*`trn'+(`ivf'+`h54')*`d54'*`g4'"
local t55 "(1-`ivf'-`h55')*`d55'*`trn'+(`ivf'+`h55')*`d55'*`g5'"
local t56 "(1-`ivf'-`h56')*`d56'*`trn'+(`ivf'+`h56')*`d56'*`g6'"
local t57 "(1-`ivf'-`h57')*`d57'*`trn'+(`ivf'+`h57')*`d57'*`g7'"
local t58 "(1-`ivf'-`h58')*`d58'*`trn'+(`ivf'+`h58')*`d58'*`g8'"
local t59 "(1-`ivf'-`h59')*`d59'*`trn'+(`ivf'+`h59')*`d59'*`g9'"
local t60 "(1-`ivf'+`h510')*`d510'*`trn'+(`ivf'-`h510')*`d510'*`g10'"
return local eq "`twr' = `t11'+`t12'+`t13'+`t14'+`t15'+`t16'+`t17'+`t18'+`t19'+`t20'+`t21'+`t22'+`t23'+`t24'+`t25'+`t26'+`t27'+`t28'+`t29'+`t30'+`t31'+`t32'+`t33'+`t34'+`t35'+`t36'+`t37'+`t38'+`t39'+`t40'+`t41'+`t42'+`t43'+`t44'+`t45'+`t46'+`t47'+`t48'+`t49'+`t50'+`t51'+`t52'+`t53'+`t54'+`t55'+`t56'+`t57'+`t58'+`t59'+`t60'"
end
nl ivfawt5: twr ivf trn e11 e12 e13 e14 e15 e16 e17 e18 e19 e110 e21 e22 e23 e24 e25 e26 e27 e28 e29 e210 e31 e32 e33 e34 e35 e36 e37 e38 e39 e310 e41 e42 e43 e44 e45 e46 e47 e48 e49 e410 e51 e52 e53 e54 e55 e56 e57 e58 e59 e510 [fw = wpop], r nocons


// Test whether beta_w for w=1,..., 10, are the same.
testnl exp(_b[/b1]) = exp(_b[/b2]) = exp(_b[/b3]) = exp(_b[/b4]) = exp(_b[/b5]) = exp(_b[/b6])  = exp(_b[/b7]) = exp(_b[/b8]) = exp(_b[/b9]) = exp(_b[/b10])
// Test whether alpha_aw for w=1,..., 10, are the same.
testnl (-0.0085 + 0.0172*exp(_b[/a11])/(1+exp(_b[/a11])))= _b[/a12] = _b[/a13] = _b[/a14] = _b[/a15] = _b[/a16]  = _b[/a17] = _b[/a18] = _b[/a19] = -(3.503*(-0.0085 + 0.0172*exp(_b[/a11])/(1+exp(_b[/a11])))+3.241*_b[/a12]+2.741*_b[/a13]+2.303*_b[/a14]+1.966*_b[/a15]+1.685*_b[/a16]+1.453*_b[/a17]+1.264*_b[/a18]+1.096*_b[/a19])
testnl _b[/a21] = _b[/a22] = _b[/a23] = _b[/a24] = _b[/a25] = _b[/a26]  = _b[/a27] = _b[/a28] = _b[/a29]  = -(1.085*_b[/a21]+1.160*_b[/a22]+1.249*_b[/a23]+1.281*_b[/a24]+1.290*_b[/a25]+1.265*_b[/a26]+1.229*_b[/a27]+1.166*_b[/a28]+1.093*_b[/a29])
testnl _b[/a31] = _b[/a32] = _b[/a33] = _b[/a34] = _b[/a35] = _b[/a36]  = _b[/a37] = _b[/a38] = _b[/a39] = -(0.604*_b[/a31]+0.653*_b[/a32]+0.768*_b[/a33]+0.859*_b[/a34]+0.937*_b[/a35]+0.996*_b[/a36]+1.027*_b[/a37]+1.055*_b[/a38]+1.045*_b[/a39])
testnl _b[/a41] = _b[/a42] = _b[/a43] = _b[/a44] = _b[/a45] = _b[/a46]  = _b[/a47] = _b[/a48] = _b[/a49] = -(0.423*_b[/a41]+0.440*_b[/a42]+0.525*_b[/a43]+0.625*_b[/a44]+0.716*_b[/a45]+0.799*_b[/a46]+0.874*_b[/a47]+0.935*_b[/a48]+0.982*_b[/a49])
testnl _b[/a51] = _b[/a52] = _b[/a53] = _b[/a54] = _b[/a55] = _b[/a56]  = _b[/a57] = _b[/a58] = _b[/a59] = -(0.255*_b[/a51]+0.248*_b[/a52]+0.313*_b[/a53]+0.390*_b[/a54]+0.447*_b[/a55]+0.523*_b[/a56]+0.608*_b[/a57]+0.706*_b[/a58]+0.840*_b[/a59])

// Obtain the estimated values and standard errors of beta_w for w=1, ... 10
* b1-10
nlcom exp(_b[/b1])
nlcom exp(_b[/b2])
nlcom exp(_b[/b3])
nlcom exp(_b[/b4])
nlcom exp(_b[/b5])
nlcom exp(_b[/b6])
nlcom exp(_b[/b7])
nlcom exp(_b[/b8])
nlcom exp(_b[/b9])
nlcom exp(_b[/b10])

// Obtain standard errors for alpha_a10
nlcom -0.0085 + 0.0172*exp(_b[/a11])/(1+exp(_b[/a11]))
nlcom -(3.503*((-0.0085 + 0.0172*exp(_b[/a11])/(1+exp(_b[/a11]))))+3.241*_b[/a12]+2.741*_b[/a13]+2.303*_b[/a14]+1.966*_b[/a15]+1.685*_b[/a16]+1.453*_b[/a17]+1.264*_b[/a18]+1.096*_b[/a19])
nlcom -(1.085*_b[/a21]+1.160*_b[/a22]+1.249*_b[/a23]+1.281*_b[/a24]+1.290*_b[/a25]+1.265*_b[/a26]+1.229*_b[/a27]+1.166*_b[/a28]+1.093*_b[/a29])
nlcom -(0.604*_b[/a31]+0.653*_b[/a32]+0.768*_b[/a33]+0.859*_b[/a34]+0.937*_b[/a35]+0.996*_b[/a36]+1.027*_b[/a37]+1.055*_b[/a38]+1.045*_b[/a39])
nlcom -(0.423*_b[/a41]+0.440*_b[/a42]+0.525*_b[/a43]+0.625*_b[/a44]+0.716*_b[/a45]+0.799*_b[/a46]+0.874*_b[/a47]+0.935*_b[/a48]+0.982*_b[/a49])
nlcom -(0.255*_b[/a51]+0.248*_b[/a52]+0.313*_b[/a53]+0.390*_b[/a54]+0.447*_b[/a55]+0.523*_b[/a56]+0.608*_b[/a57]+0.706*_b[/a58]+0.840*_b[/a59])

*** Save the estimated parameters alpha_aw & beta_w
gen a_w = .
forvalues i = 1/9{
  forvalues j = 1/5{
 replace a_w = (_b[a`j'`i':_cons]) if wr_group == `i' & age_group == `j'
 }
} 

replace a_w = -0.0085 + 0.0172*exp(_b[/a11])/(1+exp(_b[/a11])) if wr_group == 1 & age_group == 1
replace a_w = -(3.503*(-0.0085 + 0.0172*exp(_b[/a11])/(1+exp(_b[/a11])))+3.241*_b[/a12]+2.741*_b[/a13]+2.303*_b[/a14]+1.966*_b[/a15]+1.685*_b[/a16]+1.453*_b[/a17]+1.264*_b[/a18]+1.096*_b[/a19]) if wr_group == 10 & age_group == 1
replace a_w = -(1.085*_b[/a21]+1.160*_b[/a22]+1.249*_b[/a23]+1.281*_b[/a24]+1.290*_b[/a25]+1.265*_b[/a26]+1.229*_b[/a27]+1.166*_b[/a28]+1.093*_b[/a29]) if wr_group == 10 & age_group == 2
replace a_w = -(0.604*_b[/a31]+0.653*_b[/a32]+0.768*_b[/a33]+0.859*_b[/a34]+0.937*_b[/a35]+0.996*_b[/a36]+1.027*_b[/a37]+1.055*_b[/a38]+1.045*_b[/a39]) if wr_group == 10 & age_group == 3
replace a_w = -(0.423*_b[/a41]+0.440*_b[/a42]+0.525*_b[/a43]+0.625*_b[/a44]+0.716*_b[/a45]+0.799*_b[/a46]+0.874*_b[/a47]+0.935*_b[/a48]+0.982*_b[/a49]) if wr_group == 10 & age_group == 4
replace a_w = -(0.255*_b[/a51]+0.248*_b[/a52]+0.313*_b[/a53]+0.390*_b[/a54]+0.447*_b[/a55]+0.523*_b[/a56]+0.608*_b[/a57]+0.706*_b[/a58]+0.840*_b[/a59]) if wr_group == 10 & age_group == 5

gen b_w = .
forvalues i = 1/10{
 replace b_w = exp(_b[b`i':_cons])  if wr_group == `i'
 }


drop if twr == 1
// Collapse singles and multi-births within same a-w-t group into the same row
gen pred_ivf = .
replace pred_ivf = (ivf_at +a_w ) 

merge m:1 year_diff age_group wr_group using  "alljhq_twins_20082019_total_easy_esti_fin_1054_portion.dta", keepus(single b2 g2 b1f1 m3)
// Here, 5 new variables represents the # of observations in each gender composition in each a-w-t group, who are first-birth in the family. The method of calculating var1 - var5 is as same as step 2.
gen total_ivf_count = pred_ivf * (all_popu)
gen ivf_twins= .
replace ivf_twins =  (b_w * total_ivf_count)/ (b2 +g2 +b1f1 +m3)
replace ivf_twins = 0 if ivf_twins < 0
//ivf_twins represents the ivf_hat/gamma_t in Equation (3), where gamma_t need to be estimated in the MLE estimation 

forvalues i = 1/4{
 gen t_`i' = 0
 replace t_`i' = 1 if year_diff == `i'
 }
rename b2 var2
rename g2 var3
rename b1f1 var4
rename m3 var5
save "IVF_data_02.dta", replace
