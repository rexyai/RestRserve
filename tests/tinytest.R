if (requireNamespace("tinytest", quietly = TRUE)) {
  # set a seed to make the test deterministic
  set.seed(42)

  # run tests (package must be installed)
  tinytest::test_package("RestRserve")
  # tinytest::test_package(
  #   pkgname = "RestRserve",
  #   ncpu = getOption("Ncpus", 1L),
  #   side_effects = TRUE
  # )
}
