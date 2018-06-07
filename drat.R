# quick script to push to drat
build_log = system2("R", "CMD build .", stdout = TRUE)
pkg = build_log[length(build_log)]
pkg = substr(pkg, 13, nchar(pkg) - 1)
drat::insertPackage(pkg, repodir = "~/projects/drat/", commit = TRUE)

# now need to push drat:
# git push origin gh-pages
