% function [F, nn] = myEquation(vars_tilde, t, ivf_awt, d20, d02, d11,d3, delta_20, delta_02, delta_11,delta_3, n20, n02, n11, n3)
function [SS, gt_tilte, nn_type, nn_total] = myS(vars_tilde, t, ivf_awt, d20, d02, d11, d3, ...
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

    gt_all=[];
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
        moment_20 = (1 - ivf_awt(i) * gamma_t) * d20(i) + theta_20 * ivf_awt(i) * gamma_t - delta_20(i);
        moment_02 = (1 - ivf_awt(i) * gamma_t) * d02(i) + theta_02 * ivf_awt(i) * gamma_t - delta_02(i);
        moment_11 = (1 - ivf_awt(i) * gamma_t) * d11(i) + theta_11 * ivf_awt(i) * gamma_t - delta_11(i);
        moment_3 =  ((1 - ivf_awt(i) * gamma_t) * d3(i)  + theta_3  * ivf_awt(i) * gamma_t - delta_3(i));  
        if t(i) == 1
            g1ti=[moment_20; 0; 0; 0; moment_20; 0; 0; 0]';
            g1t =repmat(g1ti, n20(i), 1);
            g2ti=[0; moment_02; 0; 0; moment_02; 0; 0; 0]';
            g2t =repmat(g2ti, n02(i), 1);
            g3ti=[0; 0; moment_11; 0; moment_11; 0; 0; 0]';
            g3t =repmat(g3ti, n11(i), 1);
            g4t =[];
            if n3(i)> 0
               g4ti=[0; 0; 0; moment_3;  moment_3;  0; 0; 0]';
               g4t =repmat(g4ti, n3(i), 1);
            end
            gt=[g1t;g2t;g3t;g4t];
        elseif t(i) == 2
            g1ti=[moment_20; 0; 0; 0; 0; moment_20; 0; 0]';
            g1t =repmat(g1ti, n20(i), 1);
            g2ti=[0; moment_02; 0; 0; 0; moment_02; 0; 0]';
            g2t =repmat(g2ti, n02(i), 1);
            g3ti=[0; 0; moment_11; 0; 0; moment_11; 0; 0]';
            g3t =repmat(g3ti, n11(i), 1);
            g4t =[];
            if n3(i)> 0
               g4ti=[0; 0; 0; moment_3;  0; moment_3;  0; 0]';
               g4t =repmat(g4ti, n3(i), 1);
            end
            gt=[g1t;g2t;g3t;g4t];
        elseif t(i) == 3
            g1ti=[moment_20; 0; 0; 0; 0; 0; moment_20; 0]';
            g1t =repmat(g1ti, n20(i), 1);
            g2ti=[0; moment_02; 0; 0; 0; 0; moment_02; 0]';
            g2t =repmat(g2ti, n02(i), 1);
            g3ti=[0; 0; moment_11; 0; 0; 0; moment_11; 0]';
            g3t =repmat(g3ti, n11(i), 1);
            g4t =[];
            if n3(i)> 0
               g4ti=[0; 0; 0; moment_3;  0; 0; moment_3;  0]';
               g4t =repmat(g4ti, n3(i), 1);
            end
            gt=[g1t;g2t;g3t;g4t];
        elseif t(i) == 4
            g1ti=[moment_20; 0; 0; 0; 0; 0; 0; moment_20]';
            g1t =repmat(g1ti, n20(i), 1);
            g2ti=[0; moment_02; 0; 0; 0; 0; 0; moment_02]';
            g2t =repmat(g2ti, n02(i), 1);
            g3ti=[0; 0; moment_11; 0; 0; 0; 0; moment_11]';
            g3t =repmat(g3ti, n11(i), 1);
            g4t =[];
            if n3(i)> 0
               g4ti=[0; 0; 0; moment_3;  0; 0; 0; moment_3 ]';
               g4t =repmat(g4ti, n3(i), 1);
            end
            gt=[g1t;g2t;g3t;g4t];
        end
        gt_all=[gt_all; gt];
    end
    nn_type = sum(gt_all~=0);
    gt_bar  = sum(gt_all)./ nn_type;
    nn_total=size(gt_all,1);
    gt_tilte = (gt_all -ones(nn_total,1)*gt_bar).*(gt_all~=0);
    gt_tilteS= gt_tilte./(ones(nn_total,1)*sqrt(nn_type));
    SS= gt_tilteS' * gt_tilteS;                 
end


