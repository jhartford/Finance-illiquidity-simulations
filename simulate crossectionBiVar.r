simulate.crossSectionalBiVar = function(xMax = 5,trading.days = 250, obs.per.day = 5, seed.fixed = F, seed.value = 12345) {
  if (seed.fixed){
    set.seed(seed.value)
  }
  nobs = obs.per.day*trading.days
  market.growth = rnorm(nobs,0.00028,0.012598816)         #why generate a "different" first each time? i.e. first 1, first2 etc
  liquidity.premium = rnorm(nobs,0.001,0.02381787)
  output = data.frame(intercept = rep(NA,trading.days))
  for (illiquid.days in 1:trading.days){
    stock.growth <- 0.5*(market.growth + 1/trading.days*illiquid.days*liquidity.premium) + rnorm(nobs,0,0.025197632)
    stock.actual <- 100*cumprod(stock.growth+1)/(stock.growth[1]+1)
    stock.observed <- setPreviousValue(stock.actual,illiquid.days/trading.days)
    stock.observedGrowth <- (stock.observed-c(NA,head(stock.observed,-1)))/stock.observed
    model.ols <- lm(stock.observedGrowth ~ market.growth+liquidity.premium)
    output$illiquid.days[illiquid.days] = illiquid.days
    output$intercept[illiquid.days] = model.ols$coefficients[[1]]
    output$market.growth[illiquid.days] = model.ols$coefficients[[2]]
    output$liquidity.premium[illiquid.days] = model.ols$coefficients[[3]]
    output$market.growth.adjusted[illiquid.days] = model.ols$coefficients[[2]]*(trading.days/(trading.days-illiquid.days+1))
  }
  return(output)
}


setPreviousValue <- function(vector, probability = 0.5){
  setPreviousValue <- vector
  replace <- rbinom(n=length(vector),size=1,prob=probability)
  for (i in 2:length(vector)){
    if (replace[i] == 1) setPreviousValue[i] <- setPreviousValue[i-1]
  }
  return(setPreviousValue)
}

growth = function(vec) return (vec-c(NA,head(vec,-1)))/vec