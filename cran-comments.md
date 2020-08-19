#----------------------------------------------------------------
# v0.9.1
Response to review of v0.9.0 by Swetlana Herbrandt:

#--------
REQUEST:
"Thanks, please write the DOI in your Description field as
<doi:10.1016/j.enbuild.2013.04.022>"

RESPONSE:
Fixed.
#--------

#--------
REQUEST:
"Please do not comment out your examples and use \donttest{} instead:

\examples{
    examples for users and checks:
    executable in < 5 sec

    donttest{
        further examples for users (not used for checks)
    }
}
If you really want to show examplew resulting in error, please use
try(), i.e.

try(getse(x, 1))"

RESPONSE:
We have put the few error generating examples in \donttest{try(...)}
#--------

#--------
REQUEST:
"Please replace cat() by message() or warning() in your functions (except
for print() and summary() functions). Messages and warnings can be
suppressed if needed.

RESPONSE:
Fixed.
#--------

#--------
REQUEST:
You are changing the user's par() settings in your functions. Please
ensure with an immediate call of on.exit() that the settings are reset. E.g.
     opar <- par(no.readonly =TRUE)       # code line i
     on.exit(par(opar))                   # code line i+1

Same issue for options()."

RESPONSE:
We do see the point about setting back par() and options(). Actually it's only one function which sets par (options are not set in any functions):
setpar() is just a wrapper for changing the par values to certain values, it's
only used in plot_ts(), where the par is reset on exit. So in setpar() it can't
really reset the par, since then it would make sense to have it. setpar()
returns the current parameters, so they can be reset after plotting. So we want
to keep it.
#--------

#--------
REQUEST:
Please ensure that your functions do not modify (save or delete) the
user's home filespace in your examples/vignettes/tests. That is not
allow by CRAN policies. Please only write/save files if the user has
specified a directory. In your examples/vignettes/tests you can write to
tempdir(). I.e.

val <- lm_optim(model, D, cachedir=tempdir())

RESPONSE:
Fixed, we moved the examples into a vignette not included in the package, only
available on the accompanying website.
#--------

#--------
REQUEST:
Please fix and resubmit.

RESPONSE:
Done :)
#--------
#----------------------------------------------------------------


#----------------------------------------------------------------
# v0.9.0
We have tested on Linux 3.6.3 and 4.0.2, and on Windows 4.0.2, results are
below. Since the warnings are not the same, we think that they are related to
the particular installations, thus think it pass the CRAN server
checks. Let's see :)


#----
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
#----


#----
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

#----


#----
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
#----
#----------------------------------------------------------------
