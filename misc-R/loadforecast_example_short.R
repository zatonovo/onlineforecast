## setting work directory and libraries ####
rm(list = ls())

## Packages used
library(devtools)
library(roxygen2)

pack <- as.package("../../onlineforecast")
load_all(pack)

## Read the data
load("../data/Dbuildingheatload.rda")
Dall <- Dbuildingheatload

## Set the model output y
Dall$y <- Dall$heatload

kseq <- 0:49
Dall$AR0 <- lapply_cbind_df(kseq, function(k){
    Dall$y
})
nams(Dall$AR0) <- pst("k",kseq)

Dall$AR0



## Plot some time series
##plot_ts(Dall, patterns=c("Ta","Ws","Wd","Id|I$|Ib","^y$"), kseq=c(1,6,12,18,24,36), tstart="2011-01-01", tend="2011-01-10")

## A pairs plot
##pairs(subset(Dall, pattern="Ta", kseq=c(1:3,6,12,18)), cex=0.5)
##pairs(subset(Dall, pattern="Ta", kseq=c(1:3,6,12,18), lagforecasts=TRUE), cex=0.5)

##############################################################
## Recursive Least Squares fitting the model has inputs (a list of inputobjects)
## Define the model
##
Model <- model_class$new(output = "y",
                         inputs = list(Ta      = "Ta",#"lp(Ta, a1=0.9)",
                                       I       = "I",#"lp(I, a1=0.7)",
                                       AR0     = "AR0"#"lp(I, a1=0.7)",
##                                     mu_tday = "fs(tday/24, nharmonics=10)",
                                        #mu      = "ones()"),
                                       ),
                         MA     = c(0),
                         fitprm = "rls_prm(lambda=0.9)")

## Set the parameters which are optimized in the offlines setting
## Model$prmopt_upper <- c(Ta__a1 = 1,   I__a1 = 1,    lambda = 1) - 1e-04
## Model$prmopt_init <-  c(Ta__a1 = 0.9, I__a1 = 0.81, lambda = 0.99)
## Model$prmopt_lower <- c(Ta__a1 = 0.5, I__a1 = 0.5,  lambda = 0.9)
Model$prmopt_upper <- c(lambda = 1) - 1e-04
Model$prmopt_init <-  c(lambda = 0.99)
Model$prmopt_lower <- c(lambda = 0.8)


##############################################################
## Recursive Least Squares fitting the model has inputs (a list of inputobjects)
## The horizons to fit for
Model$kseq <- c(1)
prmopt <- Model$prmopt_init

## The fit function always initialize a new fit, no matter what is in 'Model'
Dpast <- subset(Dall, in_range("2010-12-01", Dall$t, "2011-02-01"))
## Define a logical series which sets the fitting period
Dpast$scoreperiod <- in_range("2010-12-15", Dpast$t, "2011-02-01")
## From there make a test set
Dtest <- subset(Dall, in_range("2011-02-01", Dall$t, "2011-02-03"))

## Fit to see it is working
rls_fit(prmopt = Model$prmopt_init,
        model = Model,
        data = Dpast,
        scorefun = rmse)


## This can be passed to an optimizer to optimize the offline parameters (keep them)
Model$prmopt <- optim(par = Model$prmopt_init,
                      fn = rls_fit,
                      model = Model,
                      data = Dpast,
                      scorefun = rmse,
                      lower = Model$prmopt_lower,
                      upper = Model$prmopt_upper)$par


## The model could be copied (useful for saving a particular instance)
## Model1 <- Model$clone(deep = TRUE)

## Use the optimized parameters and fit for all horizons
#Model$kseq <- 1:36
#rls_fit(Model$prmopt, Model, Dpast)

## Calculate predictions on a new data (NOTE: This uses the latest RLS parameter values, it doesn't update recursively)
#prd <- rls_predict(Model, Model$transform_data(Dtest))
#prd


## ################################################################
## Plot k step ahead prediction series
Model$kseq <- c(1)
Yhat <- rls_fit(Model$prmopt, Model, Dpast, return_analysis=TRUE)$Yhat
Dpast$yhat <- Yhat$k1

tmp <- subset(Dpast, in_range("2011-01-01", Dpast$t, "2011-01-05"))
plot(tmp$t, tmp$y, type = "l")
lines(tmp$t, tmp$yhat, type = "l", col = 2)


## ################################################################
## Plot a prediction for a particular time point
prd <- rls_predict(Model, Model$transform_data(Dtest))
##
plot(1:length(Dtest$y), Dtest$y)
##
lines(1:length(prd[1, ]), prd[1, ])
for (i in 2:nrow(prd)) {
  lines(i:(length(prd[1, ])+i-1), prd[i, ], col=i)
}


## ################################################################
## Recursive update example

## First fit on the past data (resets input and parameter states)
rls_fit(Model$prmopt, Model, Dpast)

## Recursive updating parameters and prediction
prd <- as.data.frame(matrix(NA, nrow = length(Dtest$t), ncol = length(Model$kseq)))
names(prd) <- paste0("k",Model$kseq)
##
for (it in 1:length(Dtest$t)) {
  print(paste(it,"of",length(Dtest$t)))
  ## A new datalist
  Dnew <- subset(Dtest, it)
  ## Generate the inputs
  ## Important: Note this must only be done once for new input data, since it continues from last state when lowpass filtering
  D <- Model$transform_data(Dnew)
  ## New output observations
  y <- Dnew[[Model$output]]
  ## We can now update the parameters
  rls_update(Model, D, y)
  ## and make a prediction
  prd[it, ] <- unlist(rls_predict(Model, D))
}

## Lag the predictions to match observations
prd <- lag(prd, lags = "+k")
plot(Dtest$y)
lines(prd$k8)

## Check if we get the same as when we do it in one fit
Dboth <- subset(Dall, in_range(min(Dpast$t)-3600, Dall$t, max(Dtest$t)))
Valanalysis <- rls_fit(Model$prmopt, model = Model, data = Dboth, return_analysis = TRUE)

n <- length(Dboth$t)
prd2 <- Valanalysis$Yhat[(n-nrow(prd)+1):n, ]
prd2 - prd
max(prd2 - prd, na.rm = TRUE)


## ################################################################
## But the error is auto-correlated
tmp <- rls_fit(Model$prmopt, Model, Dpast, return_analysis=TRUE)
## For 1-step ahead
Dpast$Yhat <- tmp$Yhat
Dpast$yhat <- tmp$Yhat$k1
residuals <- Dpast$y - Dpast$yhat
##
plot(residuals)
acf(residuals, na.action = na.pass)

## For 2-step ahead
Dpast$yhat <- tmp$Yhat$k2
residuals <- Dpast$y - Dpast$yhat
##
plot(residuals)
acf(residuals, na.action = na.pass)

plot_ts(Dpast$Yhat[950:1000, ], "k1$|k2$")

## Keep the residuals to use as input to an error model
## Keep the residuals to use as output
Dpast$r <- Dpast$y - tmp$Yhat$k1
## (Now use only the one-step ahead forecast as input (and output))
##Dpast$R <- Dpast$y - tmp$Yhat
Dpast$R <- matrix(Dpast$r, nrow = length(residuals), ncol = 36)
nams(Dpast$R) <- pst("k",1:36)

## See them for different horizonts (they are equal now)
plot_ts(Dpast$R[900:1000, ], "k[[:digit:]]$")

## Define a model
Me <- model_class$new(output = "r",
                      inputs = list(R = "R"),
                      fitprm = "rls_prm(lambda=0.9)")

## Set the parameters which are optimized in the offlines setting
Me$prmopt_upper <- c(lambda = 1) - 1e-04
Me$prmopt_init <-  c(lambda = 0.95)
Me$prmopt_lower <- c(lambda = 0.9)

## Set the horizons for offline optimization
Me$kseq <- c(1,2)
prmopt <- Me$prmopt_init

## Fit to check it is working
Val <- rls_fit(prmopt = Me$prmopt_init,
        model = Me,
        data = Dpast,
        scorefun = rmse,
        return_analysis = TRUE)

tmp <- cbind(r = Dpast$r, Val$Yhat)
plot_ts(tmp[which(Dpast$scoreperiod)[1000:1100], ], "*")


Dpast$ResidualsYhat <- Val$Yhat

plot_ts(subset(Dpast,Dpast$scoreperiod), "Residuals", 1:2)
plot_ts(subset(Dpast,which(Dpast$scoreperiod)[1:200]), "Residuals", 1:2)

## This can be passed to an optimizer to optimize the offline parameters (keep them)
Me$prmopt <- optim(par = Me$prmopt_init,
                   fn = rls_fit,
                   model = Me,
                   data = Dpast,
                   scorefun = rmse,
                   lower = Me$prmopt_lower,
                   upper = Me$prmopt_upper)$par


## Predict on both train and test period
## First make residuals on the entire period
Dboth <- subset(Dall, in_range(min(Dpast$t)-3600, Dall$t, max(Dtest$t)))
Yhat <- rls_fit(Model$prmopt, model = Model, data = Dboth, return_analysis = TRUE)$Yhat

Dboth$r <- Dboth$y - Yhat$k1
Dboth$R <- matrix(Dboth$r, nrow = length(Dboth$r), ncol = 36)
nams(Dboth$R) <- pst("k",1:36)

## Use the optimized parameters and fit a model for all horizons
Me$kseq <- 1:36

Rhat <- rls_fit(Me$prmopt, Me, Dboth, return_analysis = TRUE)$Yhat

scoreperiod <- in_range("2010-12-15", Dboth$t, "2011-02-01")
#itest <- in_range("2011-02-01", Dboth$t, "2011-02-03")

Yhat_r <- Dboth$y[scoreperiod] - Yhat[scoreperiod, ]
(rmse_Yhat_r <- apply(Yhat_r, 2, rmse))

## The final forecasts
Yhat_combined <- Yhat + Rhat

Rhat_r <- Dboth$y[scoreperiod] - Yhat_combined[scoreperiod, ]
(rmse_Rhat_r <- apply(Rhat_r, 2, rmse))
#apply(Dboth$r[scoreperiod] - tmp2$Yhat[scoreperiod, ], 2, rmse)

plot(rmse_Yhat_r, ylim = range(rmse_Yhat_r,rmse_Rhat_r))
points(rmse_Rhat_r, col=2)


k <- 1
plot_ts(cbind(y=Dboth$y, yhat=Yhat[ ,1], yhat_combined=Yhat_combined[ ,1])[1100:1400, ], "*")

k <- 2
plot_ts(cbind(y=Dboth$y, yhat=Yhat[ ,k], yhat_combined=Yhat_combined[ ,k])[1100:1400, ], "*")
