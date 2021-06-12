
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
                 mu_tday = "fs(tday/24, nharmonics=5)",
                 mu = "one()")
# Regression step parameters
model$add_regprm("rls_prm(lambda=0.9)")
# Optimization bounds for parameters
model$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))


# Select a model
kseq <- 2
prm <- list(I__degree = c(min=3, max=7), mu_tday__nharmonics = c(min=3, max=7))

L <- step_optim(model, D, kseq, prm, "forward")
L <- step_optim(model, D, kseq, prm, "backward")
L <- step_optim(model, D, kseq, prm, "backwardboth")
L <- step_optim(model, D, kseq, prm, "forwardboth")


# Caching (the cache files are located in the cachedir folder (folder in current working directory, can be removed with unlink(foldername))
L <- step_optim(model, D, kseq, prm, "forward", cachedir="cache", cachererun=FALSE)
L <- step_optim(model, D, kseq, prm, "backward", cachedir="cache", cachererun=FALSE)
L <- step_optim(model, D, kseq, prm, "backwardboth", cachedir="cache", cachererun=FALSE)
L <- step_optim(model, D, kseq, prm, "forwardboth", cachedir="cache", cachererun=FALSE)



# Start from a given model
modelstart <- model$clone_deep()
modelstart$inputs <- modelstart$inputs[-2:-4]
L <- step_optim(model, D, kseq, prm, "both", modelstart=modelstart)
L <- step_optim(model, D, kseq, prm, "forward", modelstart=modelstart)

# The the result in detail (L holds the selected model and it's optim result in each step)
L[[1]]
L[[2]]

# The models
getse(L, "model")
# The optim results
getse(L, "result")
#
unlist(getse(getse(L, "model"), "prm"))
sum(unlist(getse(getse(L, "result"), "counts")))




