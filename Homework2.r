library(TSA)
library(latticeExtra)
data(winnebago)
winneS<-lm(log(winnebago) ~ season(winnebago) + time(winnebago))
summary(winneS)


#3.8.a
data(retail)
xyplot(retail, ylab='Sales (in Billions)', panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.xyplot(x, y, pch = as.vector(season(retail)), col = 1)
})

#3.8.b
ret_lm <- lm(retail ~ season(retail) + time(retail))
summary(ret_lm)
plot(resid(ret_lm),type='l')

#3.8.c
xyplot(resid(ret_lm) ~ time(retail), type = "l",
       xlab = "Time", ylab = "Residuals", main='Residual Errors over Time',
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(retail)), col = 1)
       })

#3.9.a
data(prescrip)
xyplot(prescrip, ylab = "U.S. Prescription Costs",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(prescrip)), col = 1)
       })

#3.9.b
pct <- diff(prescrip) / prescrip
xyplot(pct ~ time(prescrip), type = "l",ylab = 'Percentage of Change', xlab='Time',
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(pct)), col = 1)
       })

#3.9.c
scripCos <- lm(pct ~ harmonic(pct))
summary(scripCos)
#3.9.d
xyplot(resid(pres_cos) ~ time(prescrip), type = "l",main='Residuals of Cosine Trend (1/12)',xlab='Time',ylab='Residuals')
#3.13.a
resWinneS<-resid(winneS)
#3.13.b
runs<-runs(resid(winneS))
runs
#3.13.c
acf(resWinneS)
#3.13.d
qqmath(resWinneS,main='QQ Plot of Residuals')
densityplot(resWinneS,main='Density Plot of Residuals',xlab='Residuals')
#3.14.a
ret_lm_seasonal <- lm(retail ~ time(retail) + season(retail))
retail_resid <- resid(ret_lm_seasonal)
#3.14.b
runs(retail_resid)
#3.14.c
acf(retail_resid)
#3.14.d
qqmath(retail_resid,main='QQ Plot of Residuals')
densityplot(retail_resid, main='Density Plot of Residuals',xlab='Residuals')
#3.15.a/b
presc_resid <- resid(pres_cos)
runs(presc_resid)
#3.15.c
acf(presc_resid)
#3.15.d
qqmath(presc_resid,main='QQ Plot of Residuals')
densityplot(presc_resid, main='Density Plot of Residuals',xlab='Residuals')
