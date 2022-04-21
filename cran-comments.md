## New submission

- fix CRAN failing test of the live Rserve http server (don't run it on CRAN as this error is spurious and related to the env/not being able to start process)
- import digest::digest

### Test environments

- local mac os, R 4.0.3
- Ubuntu 20.04 (gh-actions), R 4.0.3
- win-builder (devel)

### R CMD check results

There were no ERRORs/WARNINGs/NOTEs. 
