#----------------------------------------------------------------
# v1.0.2
Updated package documentation to fix:
You have file
  'onlineforecast/man/onlineforecast.Rd' with
  \docType{package}, likely intended as a package
  overview help file, but without the appropriate
  PKGNAME-package \alias as per "Documenting packages"
  in R-exts.

From email:
From: Kurt Hornik
  <Kurt.Hornik@wu.ac.at>Sent: 19 August 2023 10:53
  AMTo: Peder Bacher <pbac@dtu.dk>Cc:
  Kurt.Hornik@R-project.org
  <Kurt.Hornik@R-project.org>Subject: CRAN package
  onlineforecast


#----------------------------------------------------------------
# v1.0.1

We have changed a few minor things:
- Added better checks and error messages for data
- Small bug fixes 

#----------------------------------------------------------------
# v1.0.0

We have not changed much, but we have written a paper and submitted it to
JSS. So now we have set the major version forward.



#----------------------------------------------------------------
# v0.10.0

We have added features and done some small changes to functions. This version
should be fully backward compatible.






#----------------------------------------------------------------
# v0.9.3
# Response to review of v0.9.2 by Uwe Ligges

#--------
REQUEST:
> 1) Yes, a few times I want to show an error in the examples, should the e.g. "try(getse(x, 1))" be inside \donttest{ } ?

Not needed, you can simply run try() in zthe examples.


RESPONSE:
Fixed.
#--------



#--------
REQUEST:
> 2) I do see the point about setting back par() and options(). It's actually one function which sets par (options are not set in any functions):
>
>      - setpar() is just a wrapper for changing the par values to certain values, it's only used in plot_ts(), where the par is reset on exit. So in setpar() it can't really reset the par, since then it would make sense to have it. setpar() returns the current parameters, so they can be reset after plotting. So I think it makes sense to have it, if allowed!?

If you do

op <- setpar()
on.exit(setpar(op))

it should be fine.


RESPONSE:
Fixed.
#--------



#--------
REQUEST:
> 3) I see also the point about not writing files, however the demonstration of caching e.g. "val <- lm_optim(model, D, cachedir=tempdir())" really needs a constant path, it can't be tempdir(), since it changes the returned value between R sessions.

Yes, between R sessions, but within the session a user can inpsect the
files. Users may well change the path(). See the CRAN policies.


>  So it's only meaningful to use a constant path, like "cache" and let the user see where the cache files ends up (such that they can also understand how to remove them etc.). It's inspired by the knitr package, which does exactly the same. I can't right now figure out how to fix that in a nice way...I could comment it out and let the user uncomment, args, not a good idea ;D...well, I could remove it in the help and in the included vignettes, and put a link to the website and describe it there, do you have a better solution?


Simply always use the tempdir() in example, that way <you won't pollute
the user filespace nor overwrite fiels in user filespace.


RESPONSE:
Fixed. Only one example now write a file (in function R/cache_name.R), and it uses tempdir().
#--------
#----------------------------------------------------------------



#----------------------------------------------------------------
# v0.9.1 and v0.9.2
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
