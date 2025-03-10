function G = compute_G_analytical(theta, t, ivf_awt, d20, d02, d11, d3,...
                                  delta_20, delta_02, delta_11, delta_3, ...
                                  n20, n02, n11, n3)
    
    gamma1 = theta(1);
    gamma2 = theta(2);
    gamma3 = theta(3);
    gamma4 = theta(4);
    
    theta_20 = theta(5);
    theta_02 = theta(6);
    theta_11 = theta(7);

    % Compute theta_3
    theta_3 = 1 - theta_20 - theta_02 - theta_11;

    % Initialize G (8 moments × 7 parameters)
    G = zeros(8, 7); 

    for i = 1:length(t)
        gamma_t = theta(t(i));
    
        % compute Moment Condition
        moment_20 = n20(i) * ((1 - ivf_awt(i) * gamma_t) * d20(i) + theta_20 * ivf_awt(i) * gamma_t - delta_20(i));
        moment_02 = n02(i) * ((1 - ivf_awt(i) * gamma_t) * d02(i) + theta_02 * ivf_awt(i) * gamma_t - delta_02(i));
        moment_11 = n11(i) * ((1 - ivf_awt(i) * gamma_t) * d11(i) + theta_11 * ivf_awt(i) * gamma_t - delta_11(i));

        moment_3 = n3(i) * ((1 - ivf_awt(i) * gamma_t) * d3(i) + theta_3 * ivf_awt(i) * gamma_t - delta_3(i));  

        % Compute partial derivatives
        dMoment_20_dGamma = -n20(i) * ivf_awt(i) * d20(i) + theta_20 * ivf_awt(i) * n20(i);
        dMoment_20_dTheta20 = n20(i) * ivf_awt(i) * gamma_t;

        dMoment_02_dGamma = -n02(i) * ivf_awt(i) * d02(i) + theta_02 * ivf_awt(i) * n02(i);
        dMoment_02_dTheta02 = n02(i) * ivf_awt(i) * gamma_t;

        dMoment_11_dGamma = -n11(i) * ivf_awt(i) * d11(i) + theta_11 * ivf_awt(i) * n11(i);
        dMoment_11_dTheta11 = n11(i) * ivf_awt(i) * gamma_t;

        dMoment_3_dGamma = -n3(i) * ivf_awt(i) * d3(i) + theta_3 * ivf_awt(i) * n3(i);
        dMoment_3_dTheta20 = -n3(i) * ivf_awt(i) * gamma_t;
        dMoment_3_dTheta02 = -n3(i) * ivf_awt(i) * gamma_t;
        dMoment_3_dTheta11 = -n3(i) * ivf_awt(i) * gamma_t;

        
        G(1, t(i)) = G(1, t(i)) + dMoment_20_dGamma;
        G(2, t(i)) = G(2, t(i)) + dMoment_02_dGamma;
        G(3, t(i)) = G(3, t(i)) + dMoment_11_dGamma;
        G(4, t(i)) = G(4, t(i)) + dMoment_3_dGamma;

        G(1, 5) = G(1, 5) + dMoment_20_dTheta20;
        G(2, 6) = G(2, 6) + dMoment_02_dTheta02;
        G(3, 7) = G(3, 7) + dMoment_11_dTheta11;
        G(4, 5) = G(4, 5) + dMoment_3_dTheta20;
        G(4, 6) = G(4, 6) + dMoment_3_dTheta02;
        G(4, 7) = G(4, 7) + dMoment_3_dTheta11;

        
        dMoment_F4toF8_dGamma = dMoment_20_dGamma + dMoment_02_dGamma + dMoment_11_dGamma + dMoment_3_dGamma;

        G(5 + t(i) - 1, t(i)) = G(5 + t(i) - 1, t(i)) + dMoment_F4toF8_dGamma;
        G(5 + t(i) - 1, 5) = G(5 + t(i) - 1, 5) + dMoment_20_dTheta20+ dMoment_3_dTheta20;
        G(5 + t(i) - 1, 6) = G(5 + t(i) - 1, 6) + dMoment_02_dTheta02+ dMoment_3_dTheta02;
        G(5 + t(i) - 1, 7) = G(5 + t(i) - 1, 7) + dMoment_11_dTheta11+ dMoment_3_dTheta11;
    end

   
    n_total = sum([n20(:); n02(:); n11(:); n3(:)]);
    G = G / n_total;
end

