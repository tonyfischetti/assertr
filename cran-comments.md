
This is a minor release (3.0.1 to 3.1.0). It is mostly bug fixes, with a
small number of deliberate changes in behaviour, all listed in NEWS.

## Test environments
* local macOS 26.6 (arm64) install, R 4.5.2
* GitHub Actions: macos-latest (release), windows-latest (release),
  ubuntu-latest (devel and release)
* win-builder (devel, release, and old_release)  [TODO: not run yet]

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs  [TODO: confirm on the final commit]

## Reverse dependencies

There are 6 reverse dependencies on CRAN: canaper, data.validator,
palaeoSig and verdata (Imports), and ct and libbib (Suggests).

* data.validator (0.2.1): its test suite was run against this version
  and against assertr 3.0.1, with identical results (58 tests, no
  failures).
* canaper (1.0.1): its use of assertr was reviewed by reading its
  source. It uses `is_uniq()`, `not_na()` and `assert()` in ways this
  release does not change. Its tests were not run.
* palaeoSig (2.1-5), verdata (1.0.2) and ct (0.5.0): their use of
  assertr was reviewed by reading their source. All three only call
  `verify()`, with `has_all_names()` or plain expressions, in ways
  this release does not change. Their tests were not run.
* libbib (1.6.4, same maintainer): it marks four of its functions as
  assertr predicates. They return NA for missing values, which made
  `assert()` stop with an R error in 3.0.1 and is reported as a
  violation in this release.

The changes in this release that could affect them are:

* the default separator of `col_concat()` is now ", " (it was ""). With
  no separator, rows that differ could produce the same string, so
  `assert_rows(col_concat, is_uniq, ...)` could fail on data with no
  duplicates. The old strings are available with `sep = ""`.
* an `NA` returned by a predicate is now reported as a violation. In
  3.0.1 it stopped the assertion with an R error ("missing value where
  TRUE/FALSE needed").
* a predicate that returns a zero-length value is now an error. In
  3.0.1 the assertion passed.
* assertions now give a warning when the column selection matches no
  columns.
* the ids that tie the errors of one assertion together are now numbered
  within the session. They are no longer built from random characters,
  which changed the state of the random number generator. They still
  consist only of letters and digits, and now always start with a
  letter, so they remain usable as identifiers (data.validator uses
  them as HTML ids).
