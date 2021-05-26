# Load the package
library(onlineforecast)
# Set the data in D to simplify notation
D <- Dbuilding


# Print the first time point
D$t[1]
# Set the score period 
D$scoreperiod <- in_range("2010-12-22", D$t)
# Plot to see it
plot(D$t, D$scoreperiod, xlab="Time", ylab="Scoreperiod")

# Exclude other points example
scoreperiod2 <- D$scoreperiod
scoreperiod2[in_range("2010-12-30",D$t,"2011-01-02")] <- FALSE

# Generate new object (R6 class)
model <- forecastmodel$new()
# Set the model output
model$output = "heatload"
# Inputs (transformation step)
model$add_inputs(Ta = "Ta",
                 mu = "one()")
# Regression step parameters
model$add_regprm("rls_prm(lambda=0.9)")
# Optimization bounds for parameters
model$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))
# Set the horizons for which the model will be fitted
model$kseq <- c(3,18)


