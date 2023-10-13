function [ X ] = fed_fun(k, mu, sigma, E)
%% Calculates functional depth (m)

%% Inputs
% k is the light extinction coefficient (m-1)
% mu is the mean of ln(z), or better a lognormal distribution parameter fitted with MLE
% sigma is the standard deviation of ln(x), or better a lognormal distribution parameter fitted MLE
% E is the irradiance at the water surface (umol m-2 s-1). zfed does not change with this
%   so pick any non-zero/non-negative number. 2000 is reasonable.

%% Symbols
% z is the depth (m) 
syms z

fun= @(z)(E .* exp(-k .* z)) .* exp(-(log(z)-mu).^2./(2.*sigma.^2))./(z.*sigma.*sqrt(2.*pi));
zfed=integral(fun, 0,Inf);

X=(log(zfed)-log(E))./-k;


end

