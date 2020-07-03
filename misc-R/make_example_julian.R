## Go first and compile the package

##
library(knitr)
purl("building-electricity-load-forecast.Rmd")

## Then add files
files <- c("../../onlineforecast_0.1.0.tar.gz",
           "./Dbuilding.Rda",
           "./building-electricity-load-forecast.R")
zip("~/tmp/julianExample.zip", files, flags="-r9X-j")
