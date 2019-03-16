#Q3
data("cars")
attach(cars)
par(mfrow=c(3,1))
lines(plot(ksmooth(speed,dist ,kernel = "box",range.x = range(speed))),col="black")
lines(ksmooth(speed,dist ,kernel = "box",bandwidth = 2,range.x = range(speed)),col="red")
lines(ksmooth(speed,dist ,kernel = "box",bandwidth = 5,range.x = range(speed)),col="green")
help("lines")
legend("topright", legend = c("default","2","5"), col = c("black","red","green"),lty = 1,title = "Smoother")
# It can be observed from the graph that as we increase the band width more smooth
#the line becomes.And as we increase ithe bandwidth less data point is covered by the 
# smoothing curve

#Q2)
lines(plot(ksmooth(speed,dist ,kernel = "normal",range.x = range(speed))),col="black")
lines(ksmooth(speed,dist ,kernel = "normal",bandwidth = 2,range.x = range(speed)),col="red")
lines(ksmooth(speed,dist ,kernel = "normal",bandwidth = 5,range.x = range(speed)),col="green")
legend("topright", legend = c("default","2","5"), col = c("black","red","green"),lty = 1,title = "Smoother")
# we can see that as smooting curve with bandwidth=5 is more smooth than with bandwidth=2
# Curve with bandwidth=2 is having more spikes than with bandwidth= 5.
# So the curve becomes more smooth as we increase the band width.

#Q3)
fit<-smooth.spline(speed,dist)
plot(speed,dist)
lines(fit,col="black")
fit$df
help(predict.smooth.spline)
fit2<-predict(fit,speed,deriv=1)
# Estimated derivatives of the cubic spline is 1
