clearvars;
clc;
filename = 'C:\Users\USER\Desktop\moment condition_77.xlsx';


data = readtable(filename);
disp(data);

%Extract variables
t = data.t; 
n20 = data.n20; 
n02 = data.n02; 
n11 = data.n11; 
n3 = data.n3; 
n_total = n20 + n02 + n11+ n3; 
ivf_awt = data.ivf_awt; 
d20 = data.d20; 
d02 = data.d02; 
d11 = data.d11; 
d3 = data.d3;

% Compute delta_j
delta_20 = n20 ./ n_total;
delta_02 = n02 ./ n_total;
delta_11 = n11 ./ n_total;
delta_3 = n3 ./ n_total;


% Set upper and lower bounds 
gamma_min = 0.9;  gamma_max = 1; 
gamma2_min = 0.65;  gamma2_max = 1;
gamma3_min = 0.52; gamma3_max = 1;
gamma4_min = 0.67; gamma4_max = 1;
theta_20_min = 0.17; theta_20_max = 0.26; 
theta_02_min = 0.17; theta_02_max = 0.26; 
theta_11_min = 0.5; theta_11_max = 0.6; 

lb = [gamma_min; gamma2_min; gamma3_min; gamma4_min; theta_20_min; theta_02_min; theta_11_min]; % 下界
ub = [gamma_max; gamma2_max; gamma3_max; gamma4_max; theta_20_max; theta_02_max; theta_11_max]; % 上界


rng(360920);
num_restarts = 100;
best_solution = [];
best_obj = Inf;  



% Generate a random seed based on dataset characteristics
data_mean = mean(table2array(data));  
seed = sum(data_mean);  


% Set the random number generator seed
rng(seed);  

% Optimization loop
best_solution=zeros(7,1);
for i = 1:num_restarts
    gamma_initial = rand(1, 1) * (gamma_max - gamma_min) + gamma_min;
    gamma2_initial = rand(1, 1) * (gamma2_max - gamma2_min) + gamma2_min;
    gamma3_initial = rand(1, 1) * (gamma3_max - gamma3_min) + gamma3_min;
    gamma4_initial = rand(1, 1) * (gamma4_max - gamma4_min) + gamma4_min;
    theta_20 = rand(1) * (theta_20_max - theta_20_min) + theta_20_min;
    theta_02 = rand(1) * (theta_02_max - theta_02_min) + theta_02_min;
    theta_11 = rand(1) * (theta_11_max - theta_11_min) + theta_11_min;


    x0_tilde = [gamma_initial; gamma2_initial; gamma3_initial; gamma4_initial; theta_20; theta_02; theta_11];


    
    % Set optimization options
    options = optimoptions('fmincon', 'Display', 'iter', 'Algorithm', 'sqp', 'TolFun', 1e-6, 'TolX', 1e-6);
    


    % Define objective function
    objFun = @(vars_tilde) (myEquation(vars_tilde, t, ivf_awt, d20, d02, d11, d3, ...
                              delta_20, delta_02, delta_11, delta_3, ...
                              n20, n02, n11, n3 ));
                                 
    [solution_tilde, obj_val] = fmincon(objFun, x0_tilde, [], [], [], [], lb, ub, [], options);


    % Update the best solution if the current objective function value is smaller
    checklb = sum(solution_tilde==lb);
    checkub = sum(solution_tilde==ub);
    
    if and(obj_val<best_obj, and(checklb<1, checkub<1))
        [obj_val, best_obj];
        best_obj = obj_val;
        best_solution = solution_tilde;
    end
    gamma1A = best_solution(1);
    gamma2A = best_solution(2);
    gamma3A = best_solution(3);
    gamma4A = best_solution(4);
    
    theta_20A = best_solution(5);
    theta_02A = best_solution(6);
    theta_11A = best_solution(7);
    theta_3A  = 1-theta_20A- theta_02A-theta_11A; 

end

gamma1s = best_solution(1);
gamma2s = best_solution(2);
gamma3s = best_solution(3);
gamma4s = best_solution(4); 

theta_20s = best_solution(5);
theta_02s = best_solution(6);
theta_11s = best_solution(7);
theta_3s  = 1-theta_20s- theta_02s-theta_11s; 

[f_vals, nns]=myEquation(best_solution, t, ivf_awt, d20, d02, d11, d3, ...
                              delta_20, delta_02, delta_11, delta_3, ...
                              n20, n02, n11, n3 );
best_solution;

ww=diag(nns.^(-2));

% Display the current random seed
disp(['Using seed: ', num2str(seed)]);

%calculate se
n = length(t);

% Use best_solution (optimized parameters) as theta_hat
theta_hat = best_solution;

% calculate moment conditions g_t and S

[SS, gt_tilte, nn_type, nn_total] = my_S(theta_hat, t, ivf_awt, d20, d02, d11,d3, ...
                             delta_20, delta_02, delta_11,delta_3, ...
                             n20, n02, n11, n3);

G = compute_G_analytical(theta_hat, t, ivf_awt, d20, d02, d11, d3,...
                         delta_20, delta_02, delta_11, delta_3, ...
                         n20, n02, n11, n3);
Ga=G;
eps_g = 0;
AA = (Ga' * Ga + diag(eps_g*ones(7,1))); AAA=inv(AA'*AA)*AA;
BB = (Ga' * SS * Ga);
var_h = AAA' * BB * AAA;
sss = sqrt(diag(var_h)./(nn_type(1, [1:3, 5:8])'));
[theta_hat sss]
