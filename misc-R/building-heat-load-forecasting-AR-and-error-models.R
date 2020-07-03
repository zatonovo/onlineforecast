## ----------------------------------------------------------------
## Load the current version directly from the folder
library(devtools)
load_all(as.package("../../onlineforecast"))


## ------------------------------------------------------------------------
D <- Dbuildingheatload
D$y <- D$heatload ## Here we are missing this one! so results might not be as interesting: D$Heatload$house15
plot_ts(D, c("^y","Ta"), kseq=c(1,12))
plot_ts(D, c("^y","Ta"), "2010-12-15", "2011-01-10", kseq=c(1,12))


## ------------------------------------------------------------------------
D$scoreperiod <- period("2010-12-20", D$t)
itrain <- period(D$t, "2011-01-01")
ieval <- period("2011-01-01", D$t)


## ------------------------------------------------------------------------
## Model with no AR
model <- forecastmodel$new()
model$output = "y"
model$add_inputs(
          Ta = "lp(Ta, a1=0.9)", 
          I = "lp(I, a1=0.7)", 
          mu_tday = "fs(tday/24, nharmonics=10)",
          mu = "ones()")
model$add_regp("rls_prm(lambda=0.9)")
##
model$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999),
             I__a1 =  c(0.4, 0.8, 0.9999),
             lambda = c(0.9, 0.99, 0.9999))
##
model$kseq <- c(1,18)
model$prm <- rls_optim(model, subset(D,itrain), control=list(maxit=2))$par
##
model$kseq <- 1:36
val <- rls_fit(model$prm, model, D, returnanalysis = TRUE)


## ------------------------------------------------------------------------
## Investigate the one step residuals, lag them to match the observations
D$resid <- lag(val$Resid$k1, 1)

plot(D$t[ieval], D$resid[ieval])

acf(D$resid[ieval], na.action=na.pass)

par(mfrow=c(1,2))
plot(D$y[ieval], D$resid[ieval])
plot(D$resid[ieval], val$Resid$k1[ieval])



## ------------------------------------------------------------------------
## Add an AR part
model$add_inputs(AR = "AR(0)")
##
model$kseq <- c(1,18)
model$prm <- rls_optim(model, subset(D,itrain), control=list(maxit=2))$par
##
model$kseq <- 1:36
valAR <- rls_fit(model$prm, model, D, returnanalysis = TRUE)


## ------------------------------------------------------------------------
## An MA model for the k=1 ahead (AR on the residuals)
modelMA <- forecastmodel$new()
modelMA$output <- "resid"
modelMA$add_inputs(AR = "AR(0)")
modelMA$add_regp("rls_prm(lambda=0.9)")
##
modelMA$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))
##
modelMA$kseq <- 1
modelMA$prm <- rls_optim(modelMA, subset(D,itrain), control=list(maxit=2), cachedir = "")$par
##
valMA <- rls_fit(modelMA$prm, modelMA, D, returnanalysis = TRUE)


## ------------------------------------------------------------------------
## See the k step forecasts
ieval <- period("2011-01-01", D$t)

plot(D$t[ieval], lag(val$Resid$k1[ieval],1), type = "l", col = 1)
lines(D$t[ieval], lag(valAR$Resid$k1[ieval],1), type = "l", col = 2)
lines(D$t[ieval], lag(valMA$Resid$k1[ieval],1), type = "l", col = 3)

par(mfrow=c(1,3))
acf(val$Resid$k1[ieval], na.action=na.pass)
acf(valAR$Resid$k1[ieval], na.action=na.pass)
acf(valMA$Resid$k1[ieval], na.action=na.pass)

rmse(val$Resid$k1[ieval])
rmse(valAR$Resid$k1[ieval])
rmse(valMA$Resid$k1[ieval])



## ------------------------------------------------------------------------
## Fit the MA (AR error) model for each horizon
L <- lapply(1:36, function(k){
    ## The k step residuals from the no AR model
    D$resid <- lag(valAR$Resid[ ,k], k)
    ##
    modelMA$kseq <- k
    ##
    modelMA$prm <- rls_optim(modelMA, subset(D,itrain), control=list(maxit=2), cachedir = "")$par
    ##
    valMA <- rls_fit(modelMA$prm, modelMA, D, returnanalysis = TRUE)
    valMA$Resid
})
valMAmulti <- list()
valMAmulti$Resid <- do.call("cbind", L)



## ------------------------------------------------------------------------
## RMSE
## Only the points which are not NA for all horizons, and have values for both models
iAR <- apply(!is.na(valAR$Resid), 1, all)
iMA <- apply(!is.na(valMAmulti$Resid), 1, all)
i <- apply(!is.na(val$Resid), 1, all)
i <- iAR & iMA & i
## Only evaluation period
i <- i & ieval
##
plot(val$Resid$k1[i], type="l")
lines(valAR$Resid$k1[i], col=2)
lines(valMAmulti$Resid$k1[i], col=3)
##
tmpAR <- apply(valAR$Resid[i, ], 2, rmse)
tmpMA <- apply(valMAmulti$Resid[i, ], 2, rmse)
tmp <- apply(val$Resid[i, ], 2, rmse)
plot(tmpAR, type="b", ylim=range(tmp,tmpAR))
points(tmpMA, type="b", col=2)
points(tmp, type="b")

acf(valMAmulti$Resid$k1[i], na.action=na.pass)
pacf(valMAmulti$Resid$k1[i], na.action=na.pass)

plot(lag(val$Resid$k1[i], 1), val$Resid$k1[i])
plot(lag(valMAmulti$Resid$k1[i], 1), valMAmulti$Resid$k1[i])























## ------------------------------------------------------------------------
## RMSE
## Only the points which are not NA for all horizons, and have values for both models
iAR <- apply(!is.na(valAR$Resid), 1, all)
i <- apply(!is.na(val$Resid), 1, all)
i <- iAR & i
## Remove start
i[1:200] <- FALSE
##
tmpAR <- apply(valAR$Resid[i, ], 2, rmse)
tmp <- apply(val$Resid[i, ], 2, rmse)
plot(model$kseq, tmpAR, type="b", ylim=range(tmp,tmpAR))
points(model$kseq, tmp, type="b")

acf(val$Resid$k1[i], na.action=na.pass)
pacf(val$Resid$k1[i], na.action=na.pass)

plot(lag(val$Resid$k1[i], 1), val$Resid$k1[i])
plot(lag(val$Resid$k2[i], 2), val$Resid$k2)

y <- val$Resid$k1[i]
x <- lag(val$Resid$k1[i], 1)
fit <- lm(y ~ 0 + x)
plot(fit$residuals)
rmse(fit$residuals)
rmse(fit$residuals)

## ##
## setpar("ts", mfrow=c(ncol(val$Resid),1))
## apply(val$Resid[i, ], 2, plot, type="l")
















## Error model

In the applied model there is no auto-regressive part, hence it is almost
certain that there is auto-correlation in the residuals - which, for the shorter
horizons, can be used to improve the forecasts.

Check the auto-correlation for the one-step residuals


acf(val$Resid$k1[i], na.action=na.pass)
acf(val$Resid$k2[i], na.action=na.pass)
acf(val$Resid$k3[i], na.action=na.pass)

## Take the error from the training period

D$residual.k1 <- lag(val$Resid$k1, 1)
Dtrain$residual.k1 <- D$residual.k1[D$t %in% Dtrain$t]

##
plot_ts(Dtrain, "residual.k1")

model <- forecastmodel$new()
model$output <- "residual.k1"
model$add_inputs(
          AR = "lp(AR(c(0)), a1=0.2)")
model$add_regp("rls_prm(lambda=0.9)")
##
model$add_prmbounds(AR__a1 =  c(0.4, 0.8, 0.9999),
                    lambda = c(0.9, 0.99, 0.9999))
##
model$kseq <- c(1)
model$prm <- rls_optim(model, Dtrain, control=list(maxit=2), cachedir = "")$par
##
model$kseq <- 1:36
valMA <- rls_fit(model$prm, model, D, returnanalysis = TRUE)



## ------------------------------------------------------------------------
## RMSE
## Only the points which are not NA for all horizons, and have values for both models
iAR <- apply(!is.na(valAR$Resid), 1, all)
iMA <- apply(!is.na(valMA$Resid), 1, all)
i <- apply(!is.na(val$Resid), 1, all)
i <- iAR & iMA & i
## Remove start
i[1:200] <- FALSE
##
plot(val$Resid$k1[i], type="l")
lines(valAR$Resid$k1[i], col=2)
lines(valMA$Resid$k1[i], col=3)
##
tmpAR <- apply(valAR$Resid[i, ], 2, rmse)
tmpMA <- apply(valMA$Resid[i, ], 2, rmse)
tmp <- apply(val$Resid[i, ], 2, rmse)
plot(model$kseq, tmpAR, type="b", ylim=range(tmp,tmpAR))
points(model$kseq, tmpMA, type="b")
points(model$kseq, tmp, type="b")

acf(valMA$Resid$k1[i], na.action=na.pass)
pacf(valMA$Resid$k1[i], na.action=na.pass)

plot(lag(val$Resid$k1[i], 1), val$Resid$k1[i])
plot(lag(valMA$Resid$k1[i], 1), valMA$Resid$k1[i])
