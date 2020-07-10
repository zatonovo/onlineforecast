## Test environments
* local Fedora install, R 3.6.3

## R CMD check results
There were 1 WARNING and 1 NOTE.


One WARNING:

* checking installed package size ... NOTE
  installed size is  6.3Mb
  sub-directories of 1Mb or more:
    doc    3.0Mb
    libs   2.7Mb

So it's a little big maybe, cut can't make it smaller (it's 3 vignettes in doc)


One NOTE:

* checking compiled code ... WARNING
File ‘onlineforecast/libs/onlineforecast.so’:
  Found ‘abort’, possibly from ‘abort’ (C)
    Objects: ‘RcppExports.o’, ‘rls_update_cpp.o’
  Found ‘printf’, possibly from ‘printf’ (C)
    Objects: ‘RcppExports.o’, ‘rls_update_cpp.o’

We think that this must be related to the compiler used. We used Rcpp to include
two cpp function, and only:
include <RcppArmadillo.h>
include <Rcpp.h>

and don't do anything but matrix calculations and returning the results. So we
it must be some setting in the compiler creating this warning. Hopefully it's
not there when compiled on cran.
