#' @name RestRserveMiddleware
#' @title Creates RestRserveMiddleware
#' @description Creates RestRserveMiddleware object.
#' RestRserveMiddleware is a very useful concept which allows to perform pre-processing of requests and
#' post-processing of responses. Middleware has an access to both \code{request} and  \code{response} objects
#' and can modify them. This way each request can be checked/modified before passing hadnler and response can be
#' post processed (for example this way we developer can set up custom error messages).
#' \bold{\code{process_request} and \code{process_response} functions should
#' return \code{forward()} or \code{RestRserveResponse}}. In the former scenario \code{request}, \code{response}
#' will be passed further to subsequent middleware/handlers. In the later scenario the normal flow will be
#' immediately interrupter and result of the function will be returned as response.
#' @section Usage:
#' \itemize{
#' \item \code{app = RestRserveMiddleware$new(
#' process_request  = function(request, response) forward(),
#' process_response = function(request, response) forward(),
#' name = NULL
#' )}
#' }
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' @section Arguments:
#' \describe{
#'  \item{process_request}{\bold{function} which takes 2 arguments - \code{request} and \code{response}
#'  objects (class \code{RestRserveRequest} and \code{RestRserveResponse} correspondingly) and returns
#'  \code{forward()} or \code{RestRserveResponse}. Function is called before request is routed to handler.
#'  Usually \code{process_request} is used to perform logging, check authorization, etc.}
#'  \item{process_response}{\bold{function} which takes 2 arguments - \code{request} and \code{response}
#'  objects (of class \code{RestRserveRequest}, and \code{RestRserveResponse} correspondingly) and returns
#'  \code{forward()} or \code{RestRserveResponse}. Function is called after request is processed by handler.
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
      process_request  = function(request, response) forward(),
      process_response = function(request, response) forward(),
      name = "Middleware"
      ) {
      checkmate::assert_function(process_request, nargs = 2L)
      checkmate::assert_function(process_response, nargs = 2L)

      self$process_request  = process_request
      self$process_response = process_response
      self$name = name
    }
  )
)
