## ----------------------------------------------------------------
## Load the current version directly from the folder
library(devtools)
load_all(as.package("../../onlineforecast"))


## ------------------------------------------------------------------------
class(Dbuildingheatload)


## ------------------------------------------------------------------------
D <- Dbuildingheatload
names(D)


## ------------------------------------------------------------------------
head(D$t)


## ------------------------------------------------------------------------
head(D$Heatload[ ,1:9])


## ------------------------------------------------------------------------
head(D$Ta[ ,1:9])


## ------------------------------------------------------------------------
D$y <- D$Heatload$house15
## ## ------------------------------------------------------------------------
 plot_ts(D, c("^y","Ta"), kseq=c(1,12))


## ## ------------------------------------------------------------------------
 plot_ts(D, c("^y","Ta"), "2010-12-15", "2010-12-25", kseq=c(1,12))


## ------------------------------------------------------------------------
Dtrain <- subset(D, c("2010-12-15", "2011-01-01"))
Dtrain$scoreperiod <- in_range("2010-12-20", Dtrain$t)


## ------------------------------------------------------------------------
model <- forecastmodel$new()
model$output = "y"
model$add_inputs(
          Ta = "lp(Ta, a1=0.9)", 
          I = "lp(I, a1=0.7)", 
          mu_tday = "fs(tday/24, nharmonics=10)",
          mu = "ones()",
          AR = "lp(AR(c(0,1)), a1=0.2)")
model$add_regprm("rls_prm(lambda=0.9)")

## ## ------------------------------------------------------------------------
## model <- forecastmodel$new()
## model$output = "y"
## model$add_inputs(Ta = "lp(Ta, a1=0.9)", 
##                  I = "lp(I, a1=0.7)", 
##                  mu_tday = "fs(tday/24, nharmonics=10)",
##                  mu = "ones()",
##                  AR = "AR(c(1,2))")
## model$add_regp("rls_prm(lambda=0.9)")



## ------------------------------------------------------------------------
model$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999),
                    I__a1 =  c(0.4, 0.8, 0.9999),
                    AR__a1 =  c(0.4, 0.8, 0.9999),
                    lambda = c(0.9, 0.99, 0.9999))


model$kseq <- c(1,18)
#datatr <- model$transform_data(Dtrain)
#oi

#val <- rls_fit(model$get_prmbounds("init"), model, D, returnanalysis = TRUE)

##model2 <- forecastmodel$new()

## ---- results="hide"-----------------------------------------------------
model$kseq <- c(1,18)
model$prm <- rls_optim(model, Dtrain, control=list(maxit=2), cachedir = "building-heat-load-forecasting_cache-rls")$par


## ------------------------------------------------------------------------
model$kseq <- 1:36
val <- rls_fit(model$prm, model, D, returnanalysis = TRUE)

model$reset_state()
datatr <- model$transform_data(Dtrain)

## ---- fig.height=4-------------------------------------------------------
D$Yhat <- val$Yhat
plot_ts(D, c("^y|^Y"), "2011-01-01", "2011-02-01", kseq = c(1,18))


## ---- fig.height=4-------------------------------------------------------
i <- 200
iseq <- i+model$kseq
plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
lines(D$t[iseq], D$Yhat[i, ], type = "b", col = 2)
legend("topright", c("Observations",pst("Predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = 1:2)


## ------------------------------------------------------------------------
iseq <- which(in_range("2010-12-15",D$t,"2011-01-01"))
Dfit <- subset(D, iseq)
rls_fit(model$prm, model, Dfit)


## ------------------------------------------------------------------------
str(model$Lfits[1:2])


## ------------------------------------------------------------------------
(i <- iseq[length(iseq)] + 1)
##i <- i:(i+1)
Dnew <- subset(D, i)


## ------------------------------------------------------------------------
Dnew_transformed <- model$transform_data(Dnew)


## ------------------------------------------------------------------------
rls_update(model, Dnew_transformed, Dnew[[model$output]])


## ------------------------------------------------------------------------
yhat <- rls_predict(model, Dnew_transformed)


## ------------------------------------------------------------------------
iseq <- i+model$kseq
plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
lines(D$t[iseq], yhat, type = "b", col = 2)
legend("topright", c("observations",pst("predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = 1:2)


## ------------------------------------------------------------------------
val <- rls_fit(model$prm, model, D, returnanalysis = TRUE)
D$Yhat1 <- val$Yhat


## ------------------------------------------------------------------------
itrain <- which(in_range("2010-12-15",D$t,"2011-01-01"))
itest <- which(in_range("2011-01-01",D$t,"2011-01-04"))
rls_fit(model$prm, model, subset(D, itrain))

D$Yhat2 <- data.frame(matrix(NA, nrow(D$Yhat1), ncol(D$Yhat1)))
names(D$Yhat2) <- names(D$Yhat1)
for(i in itest){
    print(i)
    Dnew <- subset(D, i)
    Dnewtr <- model$transform_data(Dnew)
    rls_update(model, Dnewtr, Dnew[[model$output]])
    D$Yhat2[i, ] <- as.numeric(rls_predict(model, Dnewtr))
}


## ------------------------------------------------------------------------
D$Yhat1$k1[itest] - D$Yhat2$k1[itest]


valAR <- val


## ------------------------------------------------------------------------
## Compare with model with no AR
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
model$prm <- rls_optim(model, Dtrain, control=list(maxit=2), cachedir = "building-heat-load-forecasting_cache-rls")$par
##
model$kseq <- 1:36
val <- rls_fit(model$prm, model, D, returnanalysis = TRUE)


## ------------------------------------------------------------------------
## See the k step forecasts
i <- 200
iseq <- i:(i+7*24)
plot(D$t[iseq], D$y[iseq], type = "b", xlab = "t", ylab = "y")
k <- 1
lines(D$t[iseq], lag(val$Yhat[iseq,k],k), type = "b", col = 2)
lines(D$t[iseq], lag(valAR$Yhat[iseq,k],k), type = "b", col = 3)
#legend("topright", c("Observations",pst("Predictions (",min(model$kseq)," to ",max(model$kseq)," steps ahead)")), lty = 1, col = 1:2)



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
model$add_prmbounds()(AR__a1 =  c(0.4, 0.8, 0.9999),
             lambda = c(0.9, 0.99, 0.9999))
##
model$kseq <- c(1)
model$prm <- rls_optim(model, Dtrain, control=list(maxit=2))$par
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
