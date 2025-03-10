% function [F, nn] = myEquation(vars_tilde, t, ivf_awt, d20, d02, d11,d3, delta_20, delta_02, delta_11,delta_3, n20, n02, n11, n3)
function [F_val, nn] = myEquation(vars_tilde, t, ivf_awt, d20, d02, d11, d3, ...
                              delta_20, delta_02, delta_11, delta_3, ...
                              n20, n02, n11, n3)
    
    gamma1 = vars_tilde(1);
    gamma2 = vars_tilde(2);
    gamma3 = vars_tilde(3);
    gamma4 = vars_tilde(4); 

    theta_20 = vars_tilde(5);
    theta_02 = vars_tilde(6);
    theta_11 = vars_tilde(7);

    % compute theta_3
    theta_3 = 1 - theta_20 - theta_02 - theta_11;
    
    % Initialize F
    F = zeros(8,1);  

    n20T = 0;
    n02T = 0;
    n11T = 0;
    n3T = 0;
    ng1 = 0;
    ng2 = 0;
    ng3 = 0;
    ng4 = 0;

    for i = 1:length(t)
        if t(i) == 1
            gamma_t = gamma1;
        elseif t(i) == 2
            gamma_t = gamma2;
        elseif t(i) == 3
            gamma_t = gamma3;
        elseif t(i) == 4
            gamma_t = gamma4;
        end

        % compute Moment Condition
        moment_20 = n20(i) * ((1 - ivf_awt(i) * gamma_t) * d20(i) + theta_20 * ivf_awt(i) * gamma_t - delta_20(i));
        moment_02 = n02(i) * ((1 - ivf_awt(i) * gamma_t) * d02(i) + theta_02 * ivf_awt(i) * gamma_t - delta_02(i));
        moment_11 = n11(i) * ((1 - ivf_awt(i) * gamma_t) * d11(i) + theta_11 * ivf_awt(i) * gamma_t - delta_11(i));
        moment_3 =  n3(i)  * ((1 - ivf_awt(i) * gamma_t) * d3(i)  + theta_3  * ivf_awt(i) * gamma_t - delta_3(i));  

       
        F(1) = F(1) + moment_20; n20T=n20T+n20(i);
        F(2) = F(2) + moment_02; n02T=n02T+n02(i);
        F(3) = F(3) + moment_11; n11T=n11T+n11(i);
        F(4) = F(4) + moment_3;  n3T=n3T+n3(i);

       
        if t(i) == 1
            F(5) = F(5) + moment_20 + moment_02 + moment_11+ moment_3; ng1=ng1 + n20(i) +n02(i) + n11(i) + n3(i);
        elseif t(i) == 2
            F(6) = F(6) + moment_20 + moment_02 + moment_11+ moment_3; ng2=ng2 + n20(i) +n02(i) + n11(i) + n3(i);
        elseif t(i) == 3
            F(7) = F(7) + moment_20 + moment_02 + moment_11+ moment_3; ng3=ng3 + n20(i) +n02(i) + n11(i) + n3(i);
        elseif t(i) == 4
            F(8) = F(8) + moment_20 + moment_02 + moment_11+ moment_3; ng4=ng4 + n20(i) +n02(i) + n11(i) + n3(i);
        end

         % fprintf('i=%d, moment_20=%.5f, moment_02=%.5f, moment_11=%.5f\n, moment_3=%.5f\n', i, moment_20, moment_02, moment_11, moment_3);
    end
    nn = [n20T; n02T; n11T; n3T; ng1; ng2; ng3; ng4];
    % Make sure theta3 is nongative    
    pp0=0;
    if theta_3<0
        pp=10000000 * abs(theta_3-0);
        pp0=pp;
    end
    % Here, we set W = diag(nn^.(-2))
    F = F ./ nn ;
    % norm_F=norm(F);
    F_val=F'*F + pp0;
end


