## New submission

- Urgent fix. Decreased socket connectction timeout to 1 sec. This caused hang for several minutes during server start on Windows

### Test environments

- local OS X install, R 3.6.0
- Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2
- win-builder (devel)
- R-devel clang SAN (https://github.com/rocker-org/r-devel-san-clang)

###R CMD check results

There were no ERRORs/WARNINGs/NOTEs. 
