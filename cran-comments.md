## Test environments

* local Ubuntu 20.04, R 4.0.3
* rhub::check_on_solaris()
* Github Actions "windows-latest (release)"
* Github Actions "macOS-latest (release)"
* Github Actions "ubuntu-20.04-latest (release)"
* Github Actions "ubuntu-20.04-latest (devel)"

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## revdepcheck results
N/A

## CRAN team comments
Fixed failed tests on r-patched-solaris-x86. It seems sf data.frames cannot be
included as datasets because of back-compatibility issues 
(https://github.com/r-spatial/sf/issues/1419#issuecomment-640610745).
