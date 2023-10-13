function [ z ] = zeff(mu,sigma,kd )
%% Calculates efective depth (m)

%% Inputs
% mu is the mean of ln(depth), or better a lognormal distribution parameter fitted with MLE
% sigma is the standard deviation of ln(depth), or better a lognormal distribution parameter fitted with MLE
% k is the light extinction coefficient (m-1)

z=wrightOmega(mu + log((kd.*sigma.^2)) - sigma.^2)./(kd.*sigma.^2);


end

