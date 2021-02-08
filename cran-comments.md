## Test environments

* local Ubuntu 20.04, R 4.0.3
* Github Actions "windows-latest (release)"
* Github Actions "macOS-latest (release)"
* Github Actions "ubuntu-20.04-latest (release)"
* Github Actions "ubuntu-20.04-latest (devel)"

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## revdepcheck results
N/A

## CRAN team comments
One of the tests failed under r-patched-solaris-x86 because
the test was too strict and failed under small numeric differences. 
The test is relaxed now and it should work on all OS-s. 


