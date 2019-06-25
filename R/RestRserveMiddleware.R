#' @name RestRserveMiddleware
#' @title Creates RestRserveMiddleware
#' @description Creates RestRserveMiddleware object.
#' RestRserveMiddleware is a very useful concept which allows to perform pre-processing of requests and
#' post-processing of responses. Middleware has an access to both \code{request} and  \code{response} objects
#' and can modify them. This way each request can be checked/modified before passing hadnler and response can be
#' post processed (for example this way we developer can set up custom error messages).
#' \bold{\code{process_request} and \code{process_response} functions modify \code{response}
#' or throw exception using \code{HTTPError} helper.}
#' @section Usage:
#' \itemize{
#' \item \code{app = RestRserveMiddleware$new(
#' process_request  = function(request, response) TRUE,
#' process_response = function(request, response) TRUE,
#' name = NULL
#' )}
#' }
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' @section Arguments:
#' \describe{
#'  \item{process_request}{\bold{function} which takes 2 arguments - \code{request} and \code{response}
#'  objects (class \code{RestRserveRequest} and \code{RestRserveResponse} correspondingly) and modify
#'  \code{request} and \code{response} or throw exception using \code{HTTPError} helper.
#'  Function is called before request is routed to handler.
#'  Usually \code{process_request} is used to perform logging, check authorization, etc.}
#'  \item{process_response}{\bold{function} which takes 2 arguments - \code{request} and \code{response}
#'  objects (of class \code{RestRserveRequest}, and \code{RestRserveResponse} correspondingly) and modify
#'  \code{request} and \code{response} or throw exception using \code{HTTPError} helper.
#'  Function is called after request is processed by handler.
#'  Usually \code{process_response} is used to perform logging, custom error hadnling, etc.}
#' }
#' @format \code{\link{R6Class}} object.
#' @export
RestRserveMiddleware = R6::R6Class(
  classname = "RestRserveMiddleware",
  public = list(
    process_request = NULL,
    process_response = NULL,
    name = NULL,
    initialize = function(
      process_request  = function(request, response) TRUE,
      process_response = function(request, response) TRUE,
      name = "Middleware"
      ) {
      if(!is.function(process_request) || !is.function(process_response))
        stop("`process_request` and `process_response` should be functions with signature FUN(request, response)")
      if(length(formals(process_request)) != 2L)
        stop("`process_request` function should take exactly 2 arguments - request and response")
      if(length(formals(process_response)) != 2L)
        stop("`process_response` function should take exactly 2 arguments - request and response")

      self$process_request  = process_request
      self$process_response = process_response
      self$name = name
    }
  )
)
