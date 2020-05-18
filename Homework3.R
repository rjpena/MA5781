library(TSA)
library(latticeExtra)

#4.2
#(a) ?? 1 = 0.5 and ?? 2 = 0.4
a <- ARMAacf(ma = c(-0.5,-0.4), lag.max = 5)	
plot(a, x = 0:5, type = "h", ylim = c(-1.5,1.5), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an MA(2) model with coefficients 0.5 and 0.4")
abline(h=0)
#(b) ?? 1 = 1.2 and ?? 2 = ???0.7
b <- ARMAacf(ma = c(-1.2,0.7), lag.max = 5)
plot(b, x = 0:5, type = "h", ylim = c(-1.5,1.5), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an MA(2) model with coefficients 1.2 and -0.7")
abline(h=0)
#(c) ?? 1 = ???1 and ?? 2 = 0.6
c <- ARMAacf(ma = c(1,-0.6), lag.max = 5)	
plot(c, x = 0:5, type = "h", ylim = c(-1.5,1.5), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an MA(2) model with coefficients -1 and -0.6")
abline(h=0)

#4.5
#(a) ?? 1 = 0.6.
a1 <- ARMAacf(ar = 0.6, lag.max = 15)	
plot(a1, x = 0:15, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(1) model with coefficients 0.6")
abline(h=0)

#(b) ?? 1 = ???0.6.
b1 = ARMAacf(ar = -0.6, lag.max = 15)	
plot(b1, x = 0:15, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(1) model with coefficients -0.6")
abline(h=0)

#(c) ?? 1 = 0.95. (Do out to 20 lags.)
c1 <- ARMAacf(ar = 0.95, lag.max = 20)	
plot(c1, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(1) model with coefficients 0.95")
abline(h=0)

#(d) ?? 1 = 0.3
d1 <- ARMAacf(ar = 0.3, lag.max = 5)	
plot(d1, x = 0:5, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(1) model with coefficients 0.3")
abline(h=0)

#4.9

#(a) ??1 = 0.6   ??2 = 0.3
a2 <- ARMAacf(ar = c(.6,.3), lag.max = 20)
plot(a2, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(2) model with coefficients .6 and 0.3")
abline(h=0)

a2root<-polyroot(c(1,0.6,0.3))
a2ro?t

#(b) ??1 = -0.4  ??2 = 0.5
b2 <- ARMAacf(ar = c(-.4,.5), lag.max = 20)
plot(b2, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(2) model with coefficients -.4 and 0.5")
abline(h=0)

b2root<-polyroot(c(1,-.4,.5))
b2r?ot

#(c) ??1 = 1.2   ??2 = -0.7
c2 <- ARMAacf(ar = c(1.2,-0.7), lag.max = 20)
plot(c2, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(2) model with coefficients 1.2 and -0.7")
abline(h=0)

c2root<-polyroot(c(1,1.2,-0.7?)
c2root

#(d) ??1 = -1    ??2 = -0.6
d2 <- ARMAacf(ar = c(-1,-0.6), lag.max = 20)
plot(d2, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(2) model with coefficients -1 and -0.6")
abline(h=0)

d2root<-polyroot(c(1,-1,-0.6))
?2root

#(e) ??1 = 0.5   ??2 = -0.9
e2 <- ARMAacf(ar = c(.5,-0.9), lag.max = 20)
plot(e2, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(2) model with coefficients .5 and -0.9")
abline(h=0)

e2root<-polyroot(c(1,0.5,-0.9))?e2root

#(f) ??1 = -0.5  ??2 = -0.6
f2 <- ARMAacf(ar = c(-.5,-0.6), lag.max = 20)
plot(f2, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(2) model with coefficients -.5 and -.6")
abline(h=0)

f2root<-polyroot(c(1,-0.5,-0.)?
f2root

#4.19
ma6<-ARMAacf(ma = -c(0.5, -0.25, 0.125, -0.0625, 0.03125, -0.0015625))
ma6
ar1<-ARMAacf(ar = -0.5 , lag.max = 7)
ar1

#4.20
ma7<-ARMAacf(ma = -c(1, -0.5, 0.25, -0.125, 0.0625, -0.03125, 0.015625))
ma7

arima11<-ARMAacf(ar = -0.5, ma = -0.5, ?ag.max = 8)
arima11
