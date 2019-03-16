# Function Estimationl
library(MASS)
help("smooth.spline")
help("mcycle")
attach(mcycle)
fit1 <- smooth.spline(times, accel)
par(mfrow=c(1,1))
plot(times, accel, main = "data(mcycle) & smoothing spline") 
lines(fit1, col = "blue")
lines(times,fitted(lm(accel~poly(times,3)))) 
lines(times,fitted(lm(accel~poly(times,6))))
help("predict.smooth.spline")
plot(predict(fit1,x,deriv=1))

#polinomial regression does 
help(lowess) 
help(supsmu) 
help(ppr)
help("bd")
library(fields)
?BD
fit<- Tps(BD[,1:4],BD$lnya,scale.type="range") 
summary(fit)
par(mfrow=c(1,1))
plot(fit)
surface(fit)

help("Tps")