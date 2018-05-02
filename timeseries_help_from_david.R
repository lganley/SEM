## Laura Ganley
## CPR data

######################

## Packages

library(forecast)
library(ggplot2)
library(imputeTS)


######################


## Data import
cpr.ts = read.csv("cprts.csv")
x = ts(cpr.ts[,2])

## Visualize time series (ts)
autoplot(x)
# The ts has two distinct components: one low frequency/low magnitude and one high frequency/high magnitude. This cannot be modeled adequately by an ARIMA model.   


######################


## Data transformation

# Find best Box-Cox transformation of data 
BoxCox.lambda(x)
# Best lambda is almost zero --> logarithmic transform
logx = log(x)
logx[is.infinite(logx)] = NA


######################


## Find best ARIMA model for log-transformed ts (w.r.t. AICc)
fit1 = auto.arima(logx,approximation=F,stepwise=F)
summary(fit1)
# Best model = ARIMA(4,1,1)

## Inspect residuals 
e = residuals(fit1)
plot(e) # no trend
qqnorm(e) # normality assumption ok
acf(e,na.action = na.pass) 
# Mildly significant autocorrelation at lags 5 & 13. Not too worrisome
pacf(e,na.action = na.pass) 
# Significant partial autocorrelations at lags 3 & 13. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fit1) # in-sample
f = function(x,h) forecast(Arima(x,c(4,1,1)),h=h)
e = tsCV(logx,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logx),na.rm=T) # mean absolute percentage error (MAPE)
# No big degradation from in-sample to out-of-sample accuracy: good


######################


## Find best ARIMA model for (non-differenced) log-data 
fit2 = auto.arima(logx,d=0,approximation=F,stepwise=F)
summary(fit2)
# ARIMA(2,0,2)

plot(fit2) 
# Some inverse AR roots are close to unit --> AR component close to not causal
# Also: inverse AR roots close to inverse MA roots: common factors? 
# In this case the model can be simplified

plot(fit1$fitted,fit2$fitted)
abline(a=0,b=1)
# The two fits (model 1 & model 2) are highly correlated but not identical
cor(fit1$fitted,fit2$fitted,use="pair") 
 
## Residual analysis
e = residuals(fit2)
plot(e) # quadratic trend
line(which(!is.na(logx)),lm(e ~ poly(1:n,2))$fitted,col=2)
qqnorm(e) # normality assumption ok
acf(e,na.action = na.pass) 
# Significant autocorrelations at lags 6 & 13
pacf(e,na.action = na.pass) 
# Significant autocorrelations at lags 6 & 13

## Model accuracy
accuracy(fit2) # in-sample
f = function(x,h) forecast(Arima(x,c(2,0,2)),h=h)
e = tsCV(logx,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logx),na.rm=T) # mean absolute percentage error (MAPE)
# No big degradation from in-sample to out-of-sample accuracy: good


######################


## Find best ARIMA for twice-differenced log-data 
fit3 = auto.arima(logx,d=2,approximation=F,stepwise=F)
summary(fit3)
# ARIMA(0,2,4)

plot(fit3) 
# Some inverse MA roots are close to unit --> MA process close to not invertible. Possible identifiability problems. 

## Residual analysis
e = residuals(fit3)
plot(e) # no significant trend
summary(lm(e ~ I(1:n)))
qqnorm(e) # normality assumption ok
acf(e,na.action = na.pass) 
pacf(e,na.action = na.pass) 
# High (partial) autocorrelations at various lags
# Differencing the time series twice may have induced spurious correlation  

## Model accuracy
accuracy(fit3) # in-sample
f = function(x,h) forecast(Arima(x,c(0,2,4)),h=h)
e = tsCV(logx,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logx),na.rm=T) # mean absolute percentage error (MAPE)
# Serious degradation from in-sample to out-of-sample accuracy
# --> the model does not generalize well which is not surprising 
# given that the data have been differenced twice


######################

## CONCLUSIONS

# Model 1: ARIMA(4,1,1)
# Model 2: ARIMA(2,0,2)
# Model 3: ARIMA(0,2,4)

# 1) All three models have comparable in-sample accuracy 
# 2) Normality assumption is reasonable for all models. 
# 3) White noise assumption is mostly reasonable for models 1 and 2 
# although mildly significant autocorrelations (full and partial)
#  suggest that some patterns may not be captured by these models. 
# Model 2 has a slight quadratic trend in its residuals.
# WN assumption violated by model 3: spurious autocorrelation 
# in residuals due to twice differencing.  
# 3) Models 1 & 2 generalize well for out-of-sample prediction. 
# Not so for model 3.

## Overall, I would have a slight preference for model 1 with model 2 
## as a close second. Model 3 is not suitable.  


######################


## Imputation of missing data

# With model 1
x.impute1 = exp(na.kalman(logx,fit1$model))

# With model 2
x.impute2 = exp(na.kalman(logx,fit2$model))

# Plot (linear scale)
matplot(cbind(x,x.impute1,x.impute2),type="l",col=1:3,xlab="time",ylab="cpr") 
legend("topright",legend=c("Original","Model 1","Model 2"),
col=1:3,lty=1,cex=.7)
# Note that the imputations of model 1 are smoother whereas 
# those of model 2 quickly go to zero

# Plot (log scale)
matplot(cbind(x,x.impute1,x.impute2),type="l",col=1:3,
	xlab="time",ylab="cpr",log="y") 
legend("bottomright",legend=c("Original","Model 1","Model 2"),
col=1:3,lty=1,cex=.7)














