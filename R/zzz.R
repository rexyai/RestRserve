.onAttach = function(libname, pkgname) {
  # make it TRUE because only this way comments inside functions can be printed during
  # non-interactive execution (Rscript for example). Whithout comments won't be possible to parse
  # docstrings inside fucntions
  options("keep.source" = TRUE)
  recent_rserve = '1.8.6'
  if (interactive()) {
    packageStartupMessage("RestRserve is still work in progress - while we try hard to have stable API expect some breaking changes.")
    if(utils::packageVersion("Rserve") < recent_rserve) {
      m1 = sprintf("Rserve version %s detected", utils::packageVersion("Rserve"))
      m2 = sprintf("While it should work we recommend to install more recent version (>= %s) from rforge:", recent_rserve)
      m3 = "`install.packages('Rserve',,'http://www.rforge.net/')`"
      packageStartupMessage(paste(m1, m2, m3, sep = "\n"))
    }
  }
}
