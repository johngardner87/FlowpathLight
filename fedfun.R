
##############
### x is the depth (m) 
### k is the light extinction coefficient (m-1)
### mu is the mean of ln(x), a lognormal distribution parameter
### sigma is the standard deviation of ln(x), a lognormal distribution parameter
### E is the irradiance at the water surface (umol m-2 s-1). zfed does not change with this
###   so pick any non-zero/non-negative number. 2000 is reasonable.


#
zfed_df <-function(k,mu,sigma,E) {

integrand<-function(x,k,mu,sigma,E) {(E * exp(-k * x)) * exp(-(log(x)-mu)^2/(2*sigma^2))/(x*sigma*sqrt(2*pi))}
int<-integrate(integrand, lower=0, upper=Inf, E=E, k=k, mu=mu, sigma=sigma)$value

zfed<-(log(int)-log(E))/-k

return(as.data.frame(zfed))

}

#
zfed<-function(k,mu,sigma,E) {
  
  integrand<-function(x,k,mu,sigma,E) {(E * exp(-k * x)) * exp(-(log(x)-mu)^2/(2*sigma^2))/(x*sigma*sqrt(2*pi))}
  int<-integrate(integrand, lower=0, upper=Inf, E=E, k=k, mu=mu, sigma=sigma)$value
  
  zfed<-(log(int)-log(E))/-k
  
  return(as.vector(zfed))
  
}


#
zfed_weibull<-function(kd,k,L,E) {
  
  integrand<-function(x,kd,k,L,E) {(E * exp(-kd * x)) * ((k/L)*(x/L)^(k-1)*exp(-(x/L)^k))}
  
  int<-integrate(integrand, lower=0, upper=Inf, E=E, kd=kd, k=k, L=L)$value
  
  zfed<-(log(int)-log(E))/-kd
  
  return(as.vector(zfed))
  
}




 



