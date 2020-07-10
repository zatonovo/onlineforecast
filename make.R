# These packages must be installed

# For building vignettes
# install.packages("R.rsp")

# cpp matrix library
# install.packages("RcppArmadillo")

library(devtools)
library(roxygen2)

# pack <- as.package("../onlineforecast")
# load_all(pack)

# Update NAMESPACE, use this function to export all functions! (with @export, but S3methods (e.g. print.lm) will not get exported, so change it to export)
## docit <- function(){
##     document()
##     # Read
##     nm <- "NAMESPACE"
##     x <- scan(nm, what="character", sep="\n",blank.lines.skip=FALSE)
##     # Manipulate x
##     for(i in 1:length(x)){
##         if(length(grep("^S3method", x[i])) > 0){
##             x[i] <- gsub(",",".",gsub("S3method", "export", x[i]))
##         }
##      }
##     #
##     write(x, nm)
## }
## docit()

# ----------------------------------------------------------------
# Do also "R CMD check ../onlineforecast_1.0.0.tar.gz", it does give some other results!
#devtools::check()

# ----------------------------------------------------------------
# For running tests in folder "tests/testthat/"
# https://kbroman.org/pkg_primer/pages/tests.html
# http://r-pkgs.had.co.nz/tests.html
# Initialize first time the the testing framework
#use_testthat()
# Init new test
#use_test("newtest")

# # Run all tests
# test()

# # Run the examples
# run_examples()

# # Run tests in a single file
# load_all(as.package("../onlineforecast"))
# test_file("tests/testthat/test-rls-heat-load.R")

# Add new vignette
#usethis::use_vignette("setup-data")
#usethis::use_vignette("setup-and-use-model")
#usethis::use_vignette("forecast-evaluation")

# ----------------------------------------------------------------
# Build the package (remember to rebuild vignettes for release)
document()
build(".", vignettes=TRUE)


# Install it
install.packages("../onlineforecast_1.0.0.tar.gz")

library(onlineforecast)


# # ----------------------------------------------------------------
# # Load the current version directly from the folder
# docit()
# load_all(as.package("../../onlineforecast"), export_all=FALSE)

# # What is exported?
# onlineforecast::
