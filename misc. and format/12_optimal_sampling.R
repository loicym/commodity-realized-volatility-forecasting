##reproduce equation (19) of Aït-Sahalia et al. (2005) for optimal sampling frequency.
#assumes cum_4[U] = 0 with normaly distributed noise
#with s the series and a the variance of the microstructure noise.
#beware: T is expressed in years. that is one day is 1/252
#the volatility sigma is also in years i.e. annualized. 
#if the noise is 100% bid-ask spread "s", then "a" should be set at half the bid-ask spread: a = s/2
#use the average bid/ask spread
OptimalSampling19 <- function(t.year, sigma, a){
  
  #split the equation in three members for readability.
  
  first.member <- ((2 * (a^4) * t.year) / sigma^4)^(1/3)
  
  common.member <- (1 - (2 * (3 * a^4)^3) / (27 * sigma^4 * a^8 * t.year^2))^0.5
  
  second.member <- (1 - common.member)^(1 / 3)
  third.member <- (1 + common.member)^(1 / 3)
  
  delta.star <- first.member * (second.member + third.member)
  
  #return delta.star in minute:
  delta.star.min <- delta.star * 252 * 24 * 60 
  
  return(delta.star.min)
}