


# Load the current package
library("devtools")
pack <- as.package("../../onlineforecast")
load_all(pack)

# Set the data in D to simplify notation
D <- Dbuilding
# Set the score period 
D$scoreperiod <- in_range("2010-12-22", D$t)
#
D$tday <- make_tday(D$t, 2)
# Generate noise input
set.seed(83792)
D$noise <- make_input(rnorm(length(D$t)), 2)

# Generate new object (R6 class)
model <- forecastmodel$new()
# Set the model output
model$output = "heatload"
# Inputs (transformation step)
model$add_inputs(Ta = "Ta",
                 I = "bspline(tday, Boundary.knots = c(6,18), degree = 5, intercept=TRUE) %**% I",
                 noise = "noise",
                 mu_tday = "fs(tday/24, nharmonics=10)",
                 mu = "one()")
# Regression step parameters
model$add_regprm("rls_prm(lambda=0.9)")
# Optimization bounds for parameters
model$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))


# Reduce the model
object <- model
data <- D
kseq <- 2
prm <- list(I__degree = c(min=1, max=7), mu_tday__nharmonics = c(min=1, max=7))
optimfun = rls_optim
scorefun = rmse


L <- step_optim(object, data, kseq, prm, "forward", optimfun, scorefun, cachedir="cache", cachererun=FALSE)
L <- step_optim(object, data, kseq, prm, "backward", optimfun, scorefun, cachedir="cache", cachererun=FALSE)
L <- step_optim(object, data, kseq, prm, "backwardboth", optimfun, scorefun, cachedir="cache", cachererun=FALSE)
L <- step_optim(object, data, kseq, prm, "forwardboth", optimfun, scorefun, cachedir="cache", cachererun=FALSE)

unlist(getse(getse(L, "model"), "prm"))
sum(unlist(getse(getse(L, "result"), "counts")))




