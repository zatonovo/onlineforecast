% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_optim.R
\name{step_optim}
\alias{step_optim}
\title{Forward and backward model selection}
\usage{
step_optim(
  modelfull,
  data,
  prm = list(NA),
  direction = c("both", "backward", "forward", "backwardboth", "forwardboth"),
  modelstart = NA,
  keepinputs = FALSE,
  optimfun = rls_optim,
  fitfun = NA,
  scorefun = rmse,
  printout = FALSE,
  mc.cores = getOption("mc.cores", 2L),
  ...
)
}
\arguments{
\item{modelfull}{The full forecastmodel containing all inputs which will be
can be included in the selection.}

\item{data}{The data.list which holds the data on which the model is fitted.}

\item{prm}{A list of integer parameters to be stepped. Given using the same
syntax as parameters for optimization, e.g. `list(I__degree = c(min=3,
max=7))` will step the "degree" for input "I".}

\item{direction}{The direction to be used in the selection process.}

\item{modelstart}{A forecastmodel. If it's set then it will be used as the
selected model from the first step of the stepping. It should be a sub
model of the full model.}

\item{keepinputs}{If TRUE no inputs can be removed in a step, if FALSE then
any input can be removed. If given as a character vector with names of
inputs, then they cannot be removed in any step.}

\item{optimfun}{The function which will carry out the optimization in each
step.}

\item{fitfun}{A fit function, should be the same as used in optimfun(). If
provided, then the score is caculated with this function (instead of the
one called in optimfun(), hence the default is rls_fit(), which is called
in rls_optim()). Furthermore, information on complete cases are printed
and returned.}

\item{scorefun}{The score function used.}

\item{printout}{Logical. Passed on to fitting functions.}

\item{mc.cores}{The mc.cores argument of mclapply. If debugging it can be
necessary to set it to 1 to stop execution.}

\item{...}{Additional arguments which will be passed on to optimfun. For
example control how many steps}
}
\value{
A list with the result of each step:
 - '$model' is the model selected in each step
 - '$score' is the score for the model selected in each step
 - '$optimresult' the result return by the the optimfun
 - '$completecases' a logical vector (NA if fitfun argument is not given) indicating which time points were complete across all horizons and models for the particular step.
}
\description{
Forward and backward model selection
}
\details{
This function takes a model and carry out a model selection by stepping
backward, forward or in both directions.

Note that mclapply is used. In order to control the number of cores to use,
then set it, e.g. to one core `options(mc.cores=1)`, which is needed for
debugging to work.

The full model containing all inputs must be given. In each step new models
are generated, with either one removed input or one added input, and then all
the generated models are optimized and their scores compared. If any new
model have an improved score compared to the currently selected model, then
the new is selected and the process is repeated until no new improvement is
obtained.

In addition to selecting inputs, then integer parameters can also be stepped
through, e.g. the order of basis splined or the number of harmonics in
Fourier series.

The stepping process is different depending on the direction. In addition to
the full model, a starting model can be given, then the selection process
will start from that model.

If the direction is "both", which is default (same as "backwardboth") then the
stepping is:
 - In first step inputs are removed one-by-one
 - In following steps, inputs still in the model are removed one-by-one, and
   inputs not in the model are added one-by-one

If the direction is "backwards":
 - Inputs are only removed in each step

If the direction is "forwardboth":
 - In the first step all inputs are removed
 - In following steps (same as "both")

If the direction is "forward":
- In the first step all inputs are removed and from there inputs are only added
 

For stepping through integer variables in the transformation stage, then
these have to be set in the "prm" argument. The stepping process will follow
the input selection described above.

In case of missing values, especially in combination with auto-regressive
models, it can be very important to make sure that only complete cases are
included when calculating the score. By providing the `fitfun` argument then
the score will be calculated using only the complete cases across horizons
and models in each step, see the last examples.

Note, that either kseq or kseqopt must be set on the modelfull object. If kseqopt
is set, then it is used no matter the value of kseq.
}
\examples{


# The data, just a rather short period to keep running times short
D <- subset(Dbuilding, c("2010-12-15", "2011-02-01"))
# Set the score period
D$scoreperiod <- in_range("2010-12-22", D$t)
#
D$tday <- make_tday(D$t, 1:36)
# Generate an input which is just random noise, i.e. should be removed in the selection
set.seed(83792)
D$noise <- make_input(rnorm(length(D$t)), 1:36)

# The full model
model <- forecastmodel$new()
# Set the model output
model$output = "heatload"
# Inputs (transformation step)
model$add_inputs(Ta = "Ta",
                 noise = "noise",
                 mu_tday = "fs(tday/24, nharmonics=5)",
                 mu = "one()")
# Regression step parameters
model$add_regprm("rls_prm(lambda=0.9)")
# Optimization bounds for parameters
model$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))

# Select a model, in the optimization just run it for a single horizon
# Note that kseqopt could also be set
model$kseq <- 5

# Set the parameters to step on, note the 
prm <- list(mu_tday__nharmonics = c(min=3, max=7))

# Note the control argument, which is passed to optim, it's now set to few
# iterations in the offline parameter optimization (MUST be increased in real applications)
control <- list(maxit=1)

# On Windows multi cores are not supported, so for the examples use only one core
mc.cores <- 1

# Run the default selection scheme, which is "both"
# (same as "backwardboth" if no start model is given)
\donttest{L <- step_optim(model, D, prm, control=control, mc.cores=mc.cores)

# The optim value from each step is returned
getse(L, "optimresult")
getse(L,"score")

# The final model
L$final$model

# Other selection schemes
Lforward <- step_optim(model, D, prm, "forward", control=control, mc.cores=mc.cores)
Lbackward <- step_optim(model, D, prm, "backward", control=control, mc.cores=mc.cores)
Lbackwardboth <- step_optim(model, D, prm, "backwardboth", control=control, mc.cores=mc.cores)
Lforwardboth <- step_optim(model, D, prm, "forwardboth", control=control, mc.cores=mc.cores)

# It's possible avoid removing specified inputs
L <- step_optim(model, D, prm, keepinputs=c("mu","mu_tday"), control=control, mc.cores=mc.cores)

# Give a starting model
modelstart <- model$clone_deep()
modelstart$inputs[2:3] <- NULL
L <- step_optim(model, D, prm, modelstart=modelstart, control=control, mc.cores=mc.cores)

# If a fitting function is given, then it will be used for calculating the forecasts.
# Below it's the rls_fit function, so the same as used internally in rls_fit, so only 
# difference is that now ONLY on the complete cases for all models in each step are used
# when calculating the score in each step
L1 <- step_optim(model, D, prm, fitfun=rls_fit, control=control, mc.cores=mc.cores)

# The easiest way to conclude if missing values have an influence is to
# compare the selection result running with and without
L2 <- step_optim(model, D, prm, control=control, mc.cores=mc.cores)

# Compare the selected models
tmp1 <- capture.output(getse(L1, "model"))
tmp2 <- capture.output(getse(L2, "model"))
identical(tmp1, tmp2)}


# Note that caching can be really smart (the cache files are located in the
# cachedir folder (folder in current working directory, can be removed with
# unlink(foldername)) See e.g. `?rls_optim` for how the caching works
# L <- step_optim(model, D, prm, "forward", cachedir="cache", cachererun=FALSE, mc.cores=mc.cores)

}
