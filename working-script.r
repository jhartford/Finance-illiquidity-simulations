data=simulate.crossSectionalBiVar(trading.days = 252, obs.per.day = 5,seed.fixed=T)

par(mfrow=c(1,2))
plot(data$illiquid.days,data$market.growth,type="l",ylab = "Beta",xlab = "Illiquid Days")
lines(data$illiquid.days,data$liquidity.premium,type="l",col="red")

plot(data$illiquid.days,data$market.growth.adjusted,type="l",ylim=c(0,4),ylab = "Adj Beta",xlab = "Illiquid Days")
