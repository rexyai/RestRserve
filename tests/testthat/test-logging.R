context("Test Logger")

test_that("Test Logger", {
  skip_on_covr()
  log_const = RestRserve:::logging_constants
  log_const_names = names(log_const)
  for (level_logger in log_const_names) {
    #----------------
    lg = Logger$new(level = level_logger)
    #----------------
    # set up lgr logger
    lg_lgr = lgr::lgr
    layout = lgr::LayoutJson$new(
      toJSON_args = list(dataframe = 'columns', auto_unbox = TRUE, null = 'null', na = 'null')
    )
    lg_lgr$set_appenders(lgr::AppenderConsole$new(layout = layout))
    lg_lgr$set_threshold(level_logger)
    #----------------

    for (level_msg in setdiff(log_const_names, c('off', 'all'))) {
      # test built-in anf lgr loggers
      for (lg_test in c('lg', 'lg_lgr')) {
        lg_test = get(lg_test)
        out = capture.output(lg_test[[level_msg]]('message', data = list(one = 1)))
        if (isTRUE(log_const[[level_logger]] >= log_const[[level_msg]]) || is.na(log_const[[level_logger]])) {
          out = jsonlite::fromJSON(out, simplifyVector = FALSE)
          expect_identical(out[['data']], list(one = 1L))
        }
      }
    }
  }
})
