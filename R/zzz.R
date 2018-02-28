.onAttach = function(libname, pkgname) {
  recent_rserve = '1.8.5'
  if (interactive()) {
    if(packageVersion("Rserve") < recent_rserve) {
      m1 = sprintf("Rserve version %s detected", packageVersion("Rserve"))
      m2 = sprintf("While it should work we recommend to install more recent version (>= %s) from rforge:", recent_rserve)
      m3 = "`install.packages('Rserve',,'http://www.rforge.net/')`"
      packageStartupMessage(paste(m1, m2, m3, sep = "\n"))
    }
  }
}
