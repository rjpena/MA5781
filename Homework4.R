library(TSA)
# 5.12(a) SP Data
data(SP)
SP <- as.xts(SP)
SP_plot<-plot(SP, grid = TRUE)
SP_plot

# 5.12(b) 

log_SP<-log(SP)
log_plot<-plot(log_SP, ylab=expression(log(value)), grid =TRUE)
log_plot
