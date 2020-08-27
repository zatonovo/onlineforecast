#----------------------------------------------------------------
## # These packages must be installed
## install.packages("Rcpp")
## install.packages("R6")
## install.packages("splines")
## install.packages("digest")
## # cpp matrix library
## install.packages("RcppArmadillo")
## # For develop install
## install.packages("devtools")
## install.packages("roxygen2")
## # For testing and building vignettes
## install.packages("rmarkdown")
## install.packages("R.rsp")
## install.packages("data.table")
## install.packages("plotly")


#----------------------------------------------------------------
# Go
library(devtools)
library(roxygen2)

# Load the package directly
## document()
## pack <- as.package("../onlineforecast")
## load_all(pack)


# ----------------------------------------------------------------
# Misc
#
# Add new vignette
#usethis::use_vignette("setup-data")


# ----------------------------------------------------------------
# Running tests in folder "tests/testthat/"
#
# https://kbroman.org/pkg_primer/pages/tests.html
# http://r-pkgs.had.co.nz/tests.html
#
# Initialize first time the the testing framework
#use_testthat()
# Init new test
#use_test("newtest")

# # Run all tests
#document()
#test()

# # Run the examples
#run_examples()

# # Run tests in a single file
# load_all(as.package("../onlineforecast"))
# test_file("tests/testthat/test-rls-heat-load.R")


# ----------------------------------------------------------------
# Build the package
document()
build(".", vignettes=TRUE)

# Install it
install.packages("../onlineforecast_0.9.1.tar.gz")

library(onlineforecast)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Build binary package
#system("R CMD INSTALL --build ../onlineforecast_0.9.1.tar.gz")
# ----------------------------------------------------------------



# ----------------------------------------------------------------
# Test before release
devtools::check()

devtools::check_built("../onlineforecast_0.9.1.tar.gz")

# Does give different results than check() above
system("R CMD check ../onlineforecast_0.9.1.tar.gz")
unlink("onlineforecast.Rcheck/", recursive=TRUE)

# Use for more checking:
# https://docs.r-hub.io/



#-----------------
# WINDOWS:
# Install rtools
# Run in R:
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Restart R and check if rtools are found:
#Sys.which("make")

# Must have Makevars and Makevars.win in "src"
# Make the two files, find them and copy into "src"
#library("RcppArmadillo")
#RcppArmadillo.package.skeleton("tmp-pkg")


#-----------------
# Run another version of R (a linux in podman)
# see https://hub.docker.com/u/rocker

# Open terminal and "sudo su" (needed for podman to access files)
# Run "podman run --rm -d -p 8787:8787 -e ROOT=TRUE -e PASSWORD=pw -v $(pwd):/home/rstudio rocker/rstudio"
# In browser go to "localhost:8787"
# login: rstudio and pw
# Open make.R 
# Set working directory to this files directory
# Run installation of packages
# Make a cup of coffee
# Go to terminal and run:
#    "sudo apt-get install xml2 qpdf texlive"

# Other versions with
# "podman run --rm -p 8787:8787 -e PASSWORD=pw -v /home/pbac/g:/home/rstudio/g rocker/rstudio:3.6.1"







# ----------------------------------------------------------------
# OLD, not exporting everything anymore
#
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
