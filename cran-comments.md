## Test environments
* local OS X Yosemite 10.10.2 install, R 3.1.3
* ubuntu (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs
when checked locally with --no-manual

I got an email from Uwe.Ligges@R-project.org saying that assertr v0.4
(which was just accepted into CRAN a few days ago) failed with the
oldrelease (3.0.3). I was told to either fix or declare a proper version
dependency.

I fixed it, slightly incremented the version number and I am submitting it
here. This is the proper thing to do, right? Please excuse my ignorance, as
this is my first package.
