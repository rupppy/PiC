## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

This is a resubmission addressing the issues from the previous CRAN pretest.

* The package 'lidR' was archived on CRAN (2026-06-09, following the archival
  of its dependency 'rlas'). It has therefore been moved from 'Imports' to
  'Suggests'. The LAS/LAZ reading and writing routines now check for 'lidR'
  with requireNamespace() and fail gracefully with an informative message when
  it is not installed. The package's core functionality works with '.xyz' and
  '.txt' point-cloud files without 'lidR'.

* As requested, since 'lidR' is currently not on a mainstream repository, the
  DESCRIPTION now declares where it can be obtained via an
  'Additional_repositories' field pointing to the maintainer's repository
  (https://r-lidar.r-universe.dev), in accordance with the CRAN policies. Its
  use in the package is conditional (see above). We will move 'lidR' back once
  it returns to CRAN.

* Removed large leftover test-output files that had been accidentally included,
  reducing the tarball from ~30 MB to a few hundred KB.

## Notes on the previous WARNING

* Possibly misspelled words in DESCRIPTION (MLS, TLS, understory) are correct
  domain terms: TLS = Terrestrial Laser Scanning, MLS = Mobile Laser Scanning,
  and "understory" is the standard forestry term. They have been added to
  inst/WORDLIST.

* The flagged URL (https://www.gnu.org/licenses/gpl-3.0) returned a transient
  connection timeout on the check machines; the link is valid. It now points to
  the canonical https://www.gnu.org/licenses/gpl-3.0.en.html.
