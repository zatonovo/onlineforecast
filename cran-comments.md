We have tested on Linux 3.6.3 and 4.0.2, and on Windows 4.0.2, results are
below. Since the warnings are not the same, we think that they are related to
the particular installations, thus think it pass the CRAN server
checks. Let's see :)


#----------------------------------------------------------------
Fedora install, R 3.6.3:

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
#----------------------------------------------------------------


#----------------------------------------------------------------
Linux in container "rocker/rstudio" (in podman, had some permission issues, and
latex compilation problems), R 4.0.2:

Two NOTEs:

> checking installed package size ... NOTE
    installed size is  6.2Mb
    sub-directories of 1Mb or more:
      doc    3.0Mb
      libs   2.5Mb

> checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Wdate-time’ ‘-Werror=format-security’ ‘-Wformat’

#----------------------------------------------------------------


#----------------------------------------------------------------
Windows install, R 4.0.2:

One Warning and two NOTEs:

> checking compiled code ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

> checking installed package size ... NOTE
    installed size is  5.7Mb
    sub-directories of 1Mb or more:
      doc    3.0Mb
      libs   2.1Mb

> checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    'cache'

0 errors √ | 1 warning x | 2 notes x
#----------------------------------------------------------------
