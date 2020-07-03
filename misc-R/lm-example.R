rm(list = ls())
library(devtools)
load_all(as.package("../../onlineforecast"))

D <- Dbuildingheatload
names(D)
D$y <- D$heatload


plot_ts(D, c("^y","Ta"), kseq=c(1,12))




Dtrain <- subset(D, c("2010-12-15", "2011-01-01"))
Dtrain$scoreperiod <- in_range("2010-12-20", Dtrain$t)

model <- forecastmodel$new()
model$output = "y"
model$add_inputs(Ta = "lp(Ta, a1=0.9)",
                 AR = "AR(lags=c(0))",     # The ambient temperature
                 mu = "ones()") # ones() generates a matrix of ones (i.e. an intercept is included)

model$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999))

model$kseq <- c(1,18)
model$p <- lm_optim(model, Dtrain, control=list(maxit=2))$par

model$kseq <- 1:36
val <- lm_fit(model$p, model, D, returnanalysis = TRUE)

undebug(lm_fit)

names(val)
val$Lfitval$k1
val$scoreperiod
head(val[[6]])

head(val$Yhat, 20)

names(model)
D$Yhat <- val$Yhat
dev.new()
plot_ts(D, c("^y|^Y"), kseq = c(1,18))



####

model$insert_p(model$p)
datatr <- model$transform_data(D)
names(datatr)

X <- as.data.frame(subset(datatr, kseq = 1, lagforecasts = TRUE))
inputnms <- names(X)
## Add the model output to the data.frame for lm()
X[ ,model$output] <- D[[model$output]]

head(X,10)
## Generate the formula
frml <- pst(model$output, " ~ ", pst(inputnms, collapse=" + "), " - 1")
## Fit the model
fit <- lm(frml, X)

summary(fit)
head(fitted(fit))


head(predict(fit, X), 10)
head(predict(fit, Xpred))


head(X,10)
head(Xpred,10)
#k <- model$kseq[1]
#fit <- model$Lfits[[1]]
## Form the regressor matrix, don't lag
Xpred <- as.data.frame(subset(datatr, kseq = 1))
pred <- predict(fit, Xpred)
pred2 <- predict(fit, X)
head(pred)
head(pred2,10)
summary(fit)
head(Xpred)

X$Ta.k1[2]*fit$coef[1] + X$mu.k1[2]*fit$coef[2]


head(fit$residuals)
head(X$y - pred)

###

head(val$Yhat)

head(pred, 10)
head(pred2,10)

## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
fitTest <- lm(y ~ x)
predict(fitTest)
fitted(fitTest)

new <- data.frame(x = seq(-3, 3, 0.5))
predict(fitTest, new, se.fit = TRUE)



# Take data (See vignette "building-heat-load-forecasting" for better model and more details)
D <- subset(Dbuildingheatload, c("2010-12-15", "2011-01-01"))
D$y <- D$Heatload[ ,1]
# Define a model 
model <- forecastmodel$new()
model$output <- "y"
model$add_inputs(Ta = "Ta")
model$add_regp("rls_prm(lambda=0.9)")

# Before fitting the model, define which points to include in the evaluation of the score function
D$scoreperiod <- in_range("2010-12-20", D$t)
# And the sequence of horizons to fit for
model$kseq <- 1:6
 
# Now we can fit the model and get the model validation analysis data
L <- rls_fit(p = c(lambda=0.99), model = model, data = D, returnanalysis = TRUE)
names(L)
plot(L$Yhat$k1) # The one-step forecast
plot(L$Lfitval$k1) # The one-step RLS coefficient over time
# Fitting with lower lambda makes the RLS parameter change faster
L <- rls_fit(p = c(lambda=0.9), model = model, data = D, returnanalysis = TRUE)
names(L)
plot(L$Lfitval$k1) # The one-step RLS coefficient over time
# It can return a score
rls_fit(c(lambda=0.99), model, D, scorefun=rmse, returnanalysis = FALSE)
# Such that it can be passed to an optimzer (see ?rls_optim for a nice wrapper of optim)
val <- optim(c(lambda=0.99), rls_fit, model = model, data = D, scorefun = rmse, returnanalysis = FALSE, method = "L-BFGS-B", lower = 0.5, upper = 0.9999)
val$p

# See rmse as a function of horizon
val <- rls_fit(p = c(lambda=0.9), model = model, data = D, returnanalysis = TRUE, scorefun = rmse)
names(val)
head(val$scoreval, 100)
plot(val$scoreval)




# Take data (See vignette "building-heat-load-forecasting" for better model and more details)
D <- subset(Dbuildingheatload, c("2010-12-15", "2011-01-01"))
D$y <- D$Heatload[ ,1]
# Define a model 
model <- forecastmodel$new()
model$output <- "y"
model$add_inputs(Ta = "lp(Ta, a1=0.9)")
model$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999))
# Before fitting the model, define which points to include in the evaluation of the score function
D$scoreperiod <- in_range("2010-12-20", D$t)
# And the sequence of horizons to fit for
model$kseq <- 1:6
 
# Now we can fit the model and get the model validation analysis data
L <- lm_fit(p = c(Ta__a1 = 0.7), model = model, data = D, returnanalysis = TRUE)
names(L)
plot(L$Yhat$k1) # The one-step forecast

# The coefficients for each model
head(L$Lfitval)
# It can return a score
lm_fit(c(Ta__a1=0.7), model, D, scorefun=rmse, returnanalysis = FALSE)
# Such that it can be passed to an optimzer (see ?rls_optim for a nice wrapper of optim)
val <- optim(c(Ta__a1=0.7), lm_fit, model = model, data = D, scorefun = rmse, returnanalysis = FALSE, method = "L-BFGS-B", lower = 0.5, upper = 0.9999)
val$p

# See rmse as a function of horizon
val <- lm_fit(p = c(Ta__a1 = 0.7), model = model, data = D, returnanalysis = TRUE, scorefun = rmse)
names(val)
head(val$scoreval, 100)
plot(val$scoreval)
