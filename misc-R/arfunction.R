## Demo on how the AR part works in onlineforecast
rm(list = ls())
library(devtools)
load_all(as.package("../../onlineforecast"))

class(Dbuildingheatload)
D <- Dbuildingheatload
D$y <- D$heatload

plot_ts(D, c("^y","Ta"), kseq=c(0,12))
plot_ts(D, c("^y","Ta"), "2010-12-15", "2010-12-25", kseq=c(0,12))

Dtrain <- subset(D, c("2010-12-15", "2011-01-01"))
Dtrain$scoreperiod <- in_range("2010-12-20", Dtrain$t)
model <- forecastmodel$new()
model$output = "y"
model$add_inputs(Ta = "lp(Ta, a1=0.9)", 
                 I = "lp(I, a1=0.7)", 
                 mu_tday = "fs(tday/24, nharmonics=10)",
                 mu = "ones()")
model$add_regprm("rls_prm(lambda=0.9)")
model$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999),
                    I__a1 =  c(0.4, 0.8, 0.9999),
                    lambda = c(0.9, 0.99, 0.9999))
model$kseq <- c(1,18)
model$prm <- rls_optim(model, Dtrain, control=list(maxit=2), cachedir = "cache-building-heat-load-forecasting")$par
model$kseq <- 1:36
val <- rls_fit(model$prm, model, D, returnanalysis = TRUE)
D$Yhat <- val$Yhat

i <- 200
iseq <- i+model$kseq
plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
lines(D$t[iseq], D$Yhat[i, ], type = "b", col = 2)
legend("topright", c("Observations",pst("Predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = 1:2)
)
iseq <- which(in_range("2010-12-15",D$t,"2011-01-01"))
Dfit <- subset(D, iseq)
rls_fit(model$prm, model, Dfit, returnanalysis = FALSE)
(i <- iseq[length(iseq)] + 1)
Dnew <- subset(D, i)
Dnew_transformed <- model$transform_data(Dnew)
rls_update(model, Dnew_transformed, Dnew[[model$output]])
yhat <- rls_predict(model, Dnew_transformed)

iseq <- i+model$kseq
plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
lines(D$t[iseq], yhat, type = "b", col = 2)
legend("topright", c("observations",pst("predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = 1:2)

## AR part
modelAR <- forecastmodel$new()
modelAR$output = "y"
modelAR$add_inputs(Ta = "lp(Ta, a1=0.9)", 
                   I = "lp(I, a1=0.7)", 
                   mu_tday = "fs(tday/24, nharmonics=10)",
                   mu = "ones()", 
                   AR = "AR(lags=c(0))")
modelAR$add_regprm("rls_prm(lambda=0.9)")
modelAR$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999),
                      I__a1 =  c(0.4, 0.8, 0.9999),
                      lambda = c(0.9, 0.99, 0.9999))
modelAR$kseq <- c(1,18)

modelAR$prm <- rls_optim(modelAR, Dtrain, control=list(maxit=2), cachedir = "cache-building-heat-load-forecasting")$par
modelAR$kseq <- 1:36
valAR <- rls_fit(modelAR$prm, modelAR, D, returnanalysis = TRUE)
names(valAR)
names(modelAR)

D$YhatAR <- valAR$Yhat
plot_ts(D, c("^y|^Y"), "2011-01-01", "2011-02-01", kseq = c(1,18))
plot_ts(D, c("^y|^Y"), "2011-01-01", "2011-01-10", kseq = c(1,18))
i <- 200
iseq <- i+modelAR$kseq
plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
lines(D$t[iseq], D$Yhat[i, ], type = "b", col = 2)
lines(D$t[iseq], D$YhatAR[i, ], type = "b", col = 4)
legend("topright", c("Observations",pst("Predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)"),
                     pst("Predictions AR (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = c(1,2,4))

sqrt(mean( as.numeric((D$y[iseq] -  D$Yhat[i, ])^2), na.rm = T))
sqrt(mean( as.numeric((D$y[iseq] -  D$YhatAR[i, ])^2), na.rm = T))

## Prediction Data Frame
PredictionDF <- long_format(fit = valAR, Time = D$t) # long_format(fit = valAR, Time = D$t)
head(PredictionDF[2000:2500,], 50)
head(PredictionDF[(36*36):(36*36+36),],40)


i <- 200
iseq <- i+modelAR$kseq
plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
lines(D$t[iseq], D$Yhat[i, ], type = "b", col = 2)
lines(PredictionDF[PredictionDF$PredTime == D$t[i],]$Time, PredictionDF[PredictionDF$PredTime == D$t[i],]$Pred, type = "b", col = 5, cex = 0.4)
lines(D$t[iseq], D$YhatAR[i, ], type = "b", col = 4)
legend("topright", c("Observations",pst("Predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)"),
                     pst("Predictions AR (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = c(1,2,4)
)

## Selecting just the one hour forecasts
OneHourPred <- PredictionDF[PredictionDF$k == 1,]
length(OneHourPred$PredTime)
head(OneHourPred)
length(valAR$Yhat$k1)
head(valAR$Yhat$k1)

plot(OneHourPred$Time, OneHourPred$Pred, type = "l")
lines(D$t, D$y, col = "red")

## Update

## THINKABOUT WITH bigger lag and more new observation!!!

model <- forecastmodel$new()
model$output = "y"
modelAR$add_inputs(Ta = "lp(Ta, a1=0.9)", 
                   I = "lp(I, a1=0.7)", 
                   mu_tday = "fs(tday/24, nharmonics=10)",
                   mu = "ones()", 
                   AR = "AR(lags=c(0,1,4))")
modelAR$add_regprm("rls_prm(lambda=0.9)")
modelAR$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999),
                      I__a1 =  c(0.4, 0.8, 0.9999),
                      lambda = c(0.9, 0.99, 0.9999))
modelAR$kseq <- c(1,18)


iseq <- which(in_range("2010-12-15",D$t,"2011-01-01"))
Dfit <- subset(D, iseq)
rls_fit(modelAR$prm, modelAR, Dfit, returnanalysis = FALSE)
str(modelAR$Lfits[1:2])

(i <- iseq[length(iseq)] + 1)
Dnew <- subset(D, i:(i))
Dnew$y

modelAR$yAR
D$y[(i-6):i]
Dnew_transformed <- modelAR$transform_data(Dnew)

Dnew_transformed$AR.lag0
Dnew_transformed$AR.lag1
Dnew_transformed$AR.lag4


## modelAR$yAR
## rls_update(modelAR, Dnew_transformed, Dnew[[model$output]])
## yhatAR <- rls_predict(modelAR, Dnew_transformed)

## modelAR$yAR

## iseq <- i+modelAR$kseq
## plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
## lines(D$t[iseq], yhat[1,], type = "b", col = 2)
## lines(D$t[iseq], yhatAR[1,], type = "b", col = 4)
## legend("topright", c("observations",pst("predictions (",min(modelAR$kseq)," to ",max(modelAR$kseq)," steps ahead)"),
##                      pst("Predictions AR (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = c(1,2,4))

## (i <- iseq[length(iseq)] + 2)
## Dnew <- subset(D, i:(i))
## Dnew$y

## modelAR$yAR
## tail(Dfit$y)
## Dnew_transformed <- modelAR$transform_data(Dnew)

## Dnew_transformed$AR.lag3
## Dnew_transformed$AR.Order2
## Dnew_transformed$AR.Order1


## modelAR$yAR
## rls_update(modelAR, Dnew_transformed, Dnew[[model$output]])
## yhatAR <- rls_predict(modelAR, Dnew_transformed)

## modelAR$yAR

## (i <- iseq[length(iseq)] + 2)
## Dnew <- subset(D, i)
## Dnew_transformed <- model$transform_data(Dnew)
## rls_update(model, Dnew_transformed, Dnew[[model$output]])
## yhat <- rls_predict(model, Dnew_transformed)

## iseq <- i+modelAR$kseq
## plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
## lines(D$t[iseq], yhat[1,], type = "b", col = 2)
## lines(D$t[iseq], yhatAR[1,], type = "b", col = 4)
## legend("topright", c("observations",pst("predictions (",min(modelAR$kseq)," to ",max(modelAR$kseq)," steps ahead)"),
##                      pst("Predictions AR (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = c(1,2,4))
