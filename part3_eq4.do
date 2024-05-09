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
* Part 3: Estimation of the gender preference parameters of first-birth mothers 
*         who adopt IVFs (using Maximum Likelihood Estimation, MLE, in R).
*
*****

library(haven)
data <-read_dta('D:/User_Data/Downloads/IVF_data_02.dta')
 
 0.5162*0.5162*0.995
 0.4838*0.4838*0.995
 0.5162*0.4838*0.995*2
 //0.2651301 0.2328921 0.4969777 gender neutral rates of two-boy, two-girl and one-boy-and-one-girl
 log((1-0.2651301)/0.2651301)
 log((1-0.2328921)/0.2328921)
 log((1-0.4969777)/0.4969777)
 //calculate the initial value

 	m = function(param){
	
	r20 = 1/(1+exp(param[1])) //theta20
	r02 = 1/(1+exp(param[2])) //theta02
	r11 = 1/(1+exp(param[3])) //theta11
	k =    (1- r20 -r02 - r11) *  ((1- r20 -r02 - r11) >0)
	r_1 = 0.4/(1+exp(param[4]))+0.6 //gamma1
	r_2 = 0.4/(1+exp(param[5]))+0.6 //gamma2
	r_3 = 0.4/(1+exp(param[6]))+0.6 //gamma3
	r_4 = 0.4/(1+exp(param[7]))+0.6 //gamma4
    //we restrict theta, which represent probabilities, should be [0,1]; acoording to the hospital, gamma should be around 0.8; hence, we restrict them from 0.6 to 1.
	
    m1 = (r_1*data$ivf_twins)* ((r_1*data$ivf_twins) <= 1) + ((r_1*data$ivf_twins) > 1)
    m2 = (r_2*data$ivf_twins)* ((r_2*data$ivf_twins) <= 1) + ((r_2*data$ivf_twins) > 1)
    m3 = (r_3*data$ivf_twins)* ((r_3*data$ivf_twins) <= 1) + ((r_3*data$ivf_twins) > 1)
    m4 = (r_4*data$ivf_twins)* ((r_4*data$ivf_twins) <= 1) + ((r_4*data$ivf_twins) > 1)	
	//ivf_hat in equation (3), which should be under 1. We replace the values to 1 if they are larger than 1 while finding the optimization.
	
    a = ((0.0185*(1-m1)+(k)*(m1)))*(((0.0185*(1-m1)+(k)*(m1)))>0)
	b = ((0.0185*(1-m2)+(k)*(m2)))*(((0.0185*(1-m2)+(k)*(m2)))>0)
    c = ((0.0185*(1-m3)+(k)*(m3)))*(((0.0185*(1-m3)+(k)*(m3)))>0)
	d = ((0.0185*(1-m4)+(k)*(m4)))*(((0.0185*(1-m4)+(k)*(m4)))>0)
	//delta_awt of multi-births in equation (4) We restrict them larger than 0.
	
	s1=data$t_1*(data$var2*log(0.3861*(1-m1)+r20*(m1)))+data$t_1*(data$var3*log(0.3912*(1-m1)+r02*(m1)))+data$t_1*(data$var4*log(0.2042*(1-m1)+r11*(m1)))+data$t_1*(data$var5*log(a))
    s2=data$t_2*(data$var2*log(0.3861*(1-m2)+r20*(m2)))+data$t_2*(data$var3*log(0.3912*(1-m2)+r02*(m2)))+data$t_2*(data$var4*log(0.2042*(1-m2)+r11*(m2)))+data$t_2*(data$var5*log(b))
    s3=data$t_3*(data$var2*log(0.3861*(1-m3)+r20*(m3)))+data$t_3*(data$var3*log(0.3912*(1-m3)+r02*(m3)))+data$t_3*(data$var4*log(0.2042*(1-m3)+r11*(m3)))+data$t_3*(data$var5*log(c))
    s4=data$t_4*(data$var2*log(0.3861*(1-m4)+r20*(m4)))+data$t_4*(data$var3*log(0.3912*(1-m4)+r02*(m4)))+data$t_4*(data$var4*log(0.2042*(1-m4)+r11*(m4)))+data$t_4*(data$var5*log(d))
    // sum of MLE function within different year periods 
	
	fit = -sum(s1+s2+s3+s4)
    return(fit)}	
optim(c(1.019473,1.192052,0.012089,0,0,0,0),m,NULL, method = "CG",hessian = TRUE)

optim(c( 1.1178651,	1.2642828,	-0.1120005,	-1.4706953,	-1.2154984,	0.8842934,	1.0847513),m,NULL, method = "CG",hessian = TRUE)
//finding the optimization nearby the first solutions
> 1/(1+exp(1.1330914))
[1] 0.243591
> 1/(1+exp(1.2774258))
[1] 0.2179887
> 1/(1+exp(-0.1329396))
[1] 0.533186
> 0.4/(1+exp(-1.7696782))+0.6
[1] 0.9417671
> 0.4/(1+exp(-1.2525930))+0.6
[1] 0.9110994
> 0.4/(1+exp(1.1426591))+0.6
[1] 0.696733
> 0.4/(1+exp(1.1452953))+0.6
[1] 0.6965398
//the values of theta_j and gamma_t with the largest "fit" values 

//Hessian matrix
matrix A = (35438.12971,31883.64116,46571.25008,31.688798,5.46E+01,2.04E+01,2.86E+00\31883.64116,30394.89264,43084.42637,29.507028,4.76E+01,9.20E+00,-4.31E+00\46571.25008,43084.42637,64091.50256,58.762339,1.16E+02,9.66E+01,9.65E+01\31.688798,29.507028,58.76234,3.871292,0.00E+00,0.00E+00,0.00E+00\54.570113,47.587727,115.63434,0,4.55E+00,-9.09E-07,0.00E+00\20.421449,9.195954,96.58592,0,-9.09E-07,1.19E+01,9.09E-07\2.858053,-4.305161,96.48573,0,0.00E+00,9.09E-07,1.43E+01)
mat B = (inv(A))


//Likelihood ratio (LR) test
//5-1 LR test for (r20, r02, r11) = (0.2651301, 0.2328921, 0.4969777)

	m = function(param){
	
    r20 = 0.2651301
	r02 = 0.2328921
	r11 = 0.4969777
	k = 1- 0.2651301 -0.2328921 - 0.4969777
	r_1 = 0.35/(1+exp(param[1]))+0.6
	r_2 = 0.35/(1+exp(param[2]))+0.6
	r_3 = 0.35/(1+exp(param[3]))+0.6
	r_4 = 0.35/(1+exp(param[4]))+0.6

    m1 = (r_1*data$ivf_twins)* ((r_1*data$ivf_twins) <= 1) + ((r_1*data$ivf_twins) > 1)
    m2 = (r_2*data$ivf_twins)* ((r_2*data$ivf_twins) <= 1) + ((r_2*data$ivf_twins) > 1)
    m3 = (r_3*data$ivf_twins)* ((r_3*data$ivf_twins) <= 1) + ((r_3*data$ivf_twins) > 1)
    m4 = (r_4*data$ivf_twins)* ((r_4*data$ivf_twins) <= 1) + ((r_4*data$ivf_twins) > 1)	
		
    a = ((0.0185*(1-m1)+(k)*(m1)))*(((0.0185*(1-m1)+(k)*(m1)))>0)
	b = ((0.0185*(1-m2)+(k)*(m2)))*(((0.0185*(1-m2)+(k)*(m2)))>0)
    c = ((0.0185*(1-m3)+(k)*(m3)))*(((0.0185*(1-m3)+(k)*(m3)))>0)
	d = ((0.0185*(1-m4)+(k)*(m4)))*(((0.0185*(1-m4)+(k)*(m4)))>0)
	
	s1=data$t_1*(data$var2*log(0.3861*(1-m1)+r20*(m1)))+data$t_1*(data$var3*log(0.3912*(1-m1)+r02*(m1)))+data$t_1*(data$var4*log(0.2042*(1-m1)+r11*(m1)))+data$t_1*(data$var5*log(a))
    s2=data$t_2*(data$var2*log(0.3861*(1-m2)+r20*(m2)))+data$t_2*(data$var3*log(0.3912*(1-m2)+r02*(m2)))+data$t_2*(data$var4*log(0.2042*(1-m2)+r11*(m2)))+data$t_2*(data$var5*log(b))
    s3=data$t_3*(data$var2*log(0.3861*(1-m3)+r20*(m3)))+data$t_3*(data$var3*log(0.3912*(1-m3)+r02*(m3)))+data$t_3*(data$var4*log(0.2042*(1-m3)+r11*(m3)))+data$t_3*(data$var5*log(c))
    s4=data$t_4*(data$var2*log(0.3861*(1-m4)+r20*(m4)))+data$t_4*(data$var3*log(0.3912*(1-m4)+r02*(m4)))+data$t_4*(data$var4*log(0.2042*(1-m4)+r11*(m4)))+data$t_4*(data$var5*log(d))

	fit = -sum(s1+s2+s3+s4)
    return(fit)}	
	optim(c(0,0,0,0),m, NULL, method = "CG")
	
	//5-2 LR test for theta_11
	m = function(param){
	
	r20 = 1/(1+exp(param[1]))
	r02 = 1/(1+exp(param[2]))
	r11 = 0.4969777 // thereotical_11 value
	k =    (1- r20 - r02 - 0.4969777) *  ((1- r20 - r02 - 0.4969777) >0)
	r_1 = 0.4/(1+exp(param[3]))+0.6
	r_2 = 0.4/(1+exp(param[4]))+0.6
	r_3 = 0.4/(1+exp(param[5]))+0.6
	r_4 = 0.4/(1+exp(param[6]))+0.6

    m1 = (r_1*data$ivf_twins)* ((r_1*data$ivf_twins) <= 1) + ((r_1*data$ivf_twins) > 1)
    m2 = (r_2*data$ivf_twins)* ((r_2*data$ivf_twins) <= 1) + ((r_2*data$ivf_twins) > 1)
    m3 = (r_3*data$ivf_twins)* ((r_3*data$ivf_twins) <= 1) + ((r_3*data$ivf_twins) > 1)
    m4 = (r_4*data$ivf_twins)* ((r_4*data$ivf_twins) <= 1) + ((r_4*data$ivf_twins) > 1)	
		
    a = ((0.0185*(1-m1)+(k)*(m1)))*(((0.0185*(1-m1)+(k)*(m1)))>0)
	b = ((0.0185*(1-m2)+(k)*(m2)))*(((0.0185*(1-m2)+(k)*(m2)))>0)
    c = ((0.0185*(1-m3)+(k)*(m3)))*(((0.0185*(1-m3)+(k)*(m3)))>0)
	d = ((0.0185*(1-m4)+(k)*(m4)))*(((0.0185*(1-m4)+(k)*(m4)))>0)
	
	s1=data$t_1*(data$var2*log(0.3861*(1-m1)+r20*(m1)))+data$t_1*(data$var3*log(0.3912*(1-m1)+r02*(m1)))+data$t_1*(data$var4*log(0.2042*(1-m1)+r11*(m1)))+data$t_1*(data$var5*log(a))
    s2=data$t_2*(data$var2*log(0.3861*(1-m2)+r20*(m2)))+data$t_2*(data$var3*log(0.3912*(1-m2)+r02*(m2)))+data$t_2*(data$var4*log(0.2042*(1-m2)+r11*(m2)))+data$t_2*(data$var5*log(b))
    s3=data$t_3*(data$var2*log(0.3861*(1-m3)+r20*(m3)))+data$t_3*(data$var3*log(0.3912*(1-m3)+r02*(m3)))+data$t_3*(data$var4*log(0.2042*(1-m3)+r11*(m3)))+data$t_3*(data$var5*log(c))
    s4=data$t_4*(data$var2*log(0.3861*(1-m4)+r20*(m4)))+data$t_4*(data$var3*log(0.3912*(1-m4)+r02*(m4)))+data$t_4*(data$var4*log(0.2042*(1-m4)+r11*(m4)))+data$t_4*(data$var5*log(d))

	fit = -sum(s1+s2+s3+s4)
    return(fit)}	
	optim(c(1.019473,1.192052,0,0,0,0),m,NULL, method = "CG",hessian = TRUE)
	
	//5-3 LR test for theta_20 = 0.2651301
	m = function(param){
	
	r20 = 0.2651301 // thereotical_20 value
	r02 = 1/(1+exp(param[1]))
	r11 = 1/(1+exp(param[2]))
	k =    (1- 0.2651301 - r02 - r11) *  ((1- 0.2651301 - r02 - r11) >0)
	r_1 = 0.4/(1+exp(param[3]))+0.6
	r_2 = 0.4/(1+exp(param[4]))+0.6
	r_3 = 0.4/(1+exp(param[5]))+0.6
	r_4 = 0.4/(1+exp(param[6]))+0.6

    m1 = (r_1*data$ivf_twins)* ((r_1*data$ivf_twins) <= 1) + ((r_1*data$ivf_twins) > 1)
    m2 = (r_2*data$ivf_twins)* ((r_2*data$ivf_twins) <= 1) + ((r_2*data$ivf_twins) > 1)
    m3 = (r_3*data$ivf_twins)* ((r_3*data$ivf_twins) <= 1) + ((r_3*data$ivf_twins) > 1)
    m4 = (r_4*data$ivf_twins)* ((r_4*data$ivf_twins) <= 1) + ((r_4*data$ivf_twins) > 1)	
		
    a = ((0.0185*(1-m1)+(k)*(m1)))*(((0.0185*(1-m1)+(k)*(m1)))>0)
	b = ((0.0185*(1-m2)+(k)*(m2)))*(((0.0185*(1-m2)+(k)*(m2)))>0)
    c = ((0.0185*(1-m3)+(k)*(m3)))*(((0.0185*(1-m3)+(k)*(m3)))>0)
	d = ((0.0185*(1-m4)+(k)*(m4)))*(((0.0185*(1-m4)+(k)*(m4)))>0)
	
	s1=data$t_1*(data$var2*log(0.3861*(1-m1)+r20*(m1)))+data$t_1*(data$var3*log(0.3912*(1-m1)+r02*(m1)))+data$t_1*(data$var4*log(0.2042*(1-m1)+r11*(m1)))+data$t_1*(data$var5*log(a))
    s2=data$t_2*(data$var2*log(0.3861*(1-m2)+r20*(m2)))+data$t_2*(data$var3*log(0.3912*(1-m2)+r02*(m2)))+data$t_2*(data$var4*log(0.2042*(1-m2)+r11*(m2)))+data$t_2*(data$var5*log(b))
    s3=data$t_3*(data$var2*log(0.3861*(1-m3)+r20*(m3)))+data$t_3*(data$var3*log(0.3912*(1-m3)+r02*(m3)))+data$t_3*(data$var4*log(0.2042*(1-m3)+r11*(m3)))+data$t_3*(data$var5*log(c))
    s4=data$t_4*(data$var2*log(0.3861*(1-m4)+r20*(m4)))+data$t_4*(data$var3*log(0.3912*(1-m4)+r02*(m4)))+data$t_4*(data$var4*log(0.2042*(1-m4)+r11*(m4)))+data$t_4*(data$var5*log(d))

	fit = -sum(s1+s2+s3+s4)
    return(fit)}	
	optim(c(1.192052,0.012089,0,0,0,0),m,NULL, method = "CG",hessian = TRUE)
	
	//5-4 LR test for theta_02=0.2328921
	m = function(param){
	
	r20 = 1/(1+exp(param[1]))
	r02 = 0.2328921 // thereotical_02 value
	r11 = 1/(1+exp(param[2]))
	k =    (1- r20 - 0.2328921 - r11) *  ((1- r20 - 0.2328921- r11) >0)
	r_1 = 0.4/(1+exp(param[3]))+0.6
	r_2 = 0.4/(1+exp(param[4]))+0.6
	r_3 = 0.4/(1+exp(param[5]))+0.6
	r_4 = 0.4/(1+exp(param[6]))+0.6

    m1 = (r_1*data$ivf_twins)* ((r_1*data$ivf_twins) <= 1) + ((r_1*data$ivf_twins) > 1)
    m2 = (r_2*data$ivf_twins)* ((r_2*data$ivf_twins) <= 1) + ((r_2*data$ivf_twins) > 1)
    m3 = (r_3*data$ivf_twins)* ((r_3*data$ivf_twins) <= 1) + ((r_3*data$ivf_twins) > 1)
    m4 = (r_4*data$ivf_twins)* ((r_4*data$ivf_twins) <= 1) + ((r_4*data$ivf_twins) > 1)	
		
    a = ((0.0185*(1-m1)+(k)*(m1)))*(((0.0185*(1-m1)+(k)*(m1)))>0)
	b = ((0.0185*(1-m2)+(k)*(m2)))*(((0.0185*(1-m2)+(k)*(m2)))>0)
    c = ((0.0185*(1-m3)+(k)*(m3)))*(((0.0185*(1-m3)+(k)*(m3)))>0)
	d = ((0.0185*(1-m4)+(k)*(m4)))*(((0.0185*(1-m4)+(k)*(m4)))>0)
	
	s1=data$t_1*(data$var2*log(0.3861*(1-m1)+r20*(m1)))+data$t_1*(data$var3*log(0.3912*(1-m1)+r02*(m1)))+data$t_1*(data$var4*log(0.2042*(1-m1)+r11*(m1)))+data$t_1*(data$var5*log(a))
    s2=data$t_2*(data$var2*log(0.3861*(1-m2)+r20*(m2)))+data$t_2*(data$var3*log(0.3912*(1-m2)+r02*(m2)))+data$t_2*(data$var4*log(0.2042*(1-m2)+r11*(m2)))+data$t_2*(data$var5*log(b))
    s3=data$t_3*(data$var2*log(0.3861*(1-m3)+r20*(m3)))+data$t_3*(data$var3*log(0.3912*(1-m3)+r02*(m3)))+data$t_3*(data$var4*log(0.2042*(1-m3)+r11*(m3)))+data$t_3*(data$var5*log(c))
    s4=data$t_4*(data$var2*log(0.3861*(1-m4)+r20*(m4)))+data$t_4*(data$var3*log(0.3912*(1-m4)+r02*(m4)))+data$t_4*(data$var4*log(0.2042*(1-m4)+r11*(m4)))+data$t_4*(data$var5*log(d))

	fit = -sum(s1+s2+s3+s4)
    return(fit)}	
	optim(c(1.019473,0.012089,0,0,0,0),m,NULL, method = "CG",hessian = TRUE)
	
	//5-5. LR test for the null that all gamma's are the same
	m = function(param){
	
	r20 = 1/(1+exp(param[1]))
	r02 = 1/(1+exp(param[2]))
	r11 = 1/(1+exp(param[3]))
	k =    (1- r20 -r02 - r11) *  ((1- r20 -r02 - r11) >0)
	r_1 = 0.4/(1+exp(param[4]))+0.6 
	r_2 = 0.4/(1+exp(param[4]))+0.6
	r_3 = 0.4/(1+exp(param[4]))+0.6
	r_4 = 0.4/(1+exp(param[4]))+0.6

    m1 = (r_1*data$ivf_twins)* ((r_1*data$ivf_twins) <= 1) + ((r_1*data$ivf_twins) > 1)
    m2 = (r_2*data$ivf_twins)* ((r_2*data$ivf_twins) <= 1) + ((r_2*data$ivf_twins) > 1)
    m3 = (r_3*data$ivf_twins)* ((r_3*data$ivf_twins) <= 1) + ((r_3*data$ivf_twins) > 1)
    m4 = (r_4*data$ivf_twins)* ((r_4*data$ivf_twins) <= 1) + ((r_4*data$ivf_twins) > 1)	
		
    a = ((0.0185*(1-m1)+(k)*(m1)))*(((0.0248*(1-m1)+(k)*(m1)))>0)
	b = ((0.0185*(1-m2)+(k)*(m2)))*(((0.0248*(1-m2)+(k)*(m2)))>0)
    c = ((0.0185*(1-m3)+(k)*(m3)))*(((0.0248*(1-m3)+(k)*(m3)))>0)
	d = ((0.0185*(1-m4)+(k)*(m4)))*(((0.0248*(1-m4)+(k)*(m4)))>0)
	
	s1=data$t_1*(data$var2*log(0.3861*(1-m1)+r20*(m1)))+data$t_1*(data$var3*log(0.3912*(1-m1)+r02*(m1)))+data$t_1*(data$var4*log(0.2042*(1-m1)+r11*(m1)))+data$t_1*(data$var5*log(a))
    s2=data$t_2*(data$var2*log(0.3861*(1-m2)+r20*(m2)))+data$t_2*(data$var3*log(0.3912*(1-m2)+r02*(m2)))+data$t_2*(data$var4*log(0.2042*(1-m2)+r11*(m2)))+data$t_2*(data$var5*log(b))
    s3=data$t_3*(data$var2*log(0.3861*(1-m3)+r20*(m3)))+data$t_3*(data$var3*log(0.3912*(1-m3)+r02*(m3)))+data$t_3*(data$var4*log(0.2042*(1-m3)+r11*(m3)))+data$t_3*(data$var5*log(c))
    s4=data$t_4*(data$var2*log(0.3861*(1-m4)+r20*(m4)))+data$t_4*(data$var3*log(0.3912*(1-m4)+r02*(m4)))+data$t_4*(data$var4*log(0.2042*(1-m4)+r11*(m4)))+data$t_4*(data$var5*log(d))

	fit = -sum(s1+s2+s3+s4)
    return(fit)}	
	optim(c(1.019473,1.192052,0.012089,0),m, NULL, method = "CG")
