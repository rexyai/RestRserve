% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ETagMiddleware.R
\name{ETagMiddleware}
\alias{ETagMiddleware}
\title{Creates ETag middleware object}
\description{
Adds ETag to an \link{Application}. \cr

ETags are header information that enable the caching of content.
If enabled, RestRserve will return an ETag (eg a hash of a file) alongside
the last time it was modified.
When a request is sent, additional headers such as
\href{https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match}{\code{If-None-Match}},
\href{https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Match}{\code{If-Match}},
\href{https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since}{\code{If-Modified-Since}},
and
\href{https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-UnModified-Since}{\code{If-Unmodified-Since}},
can be passed to the server as well.

If the conditions are met (different hash in case of a \code{If-None-Match} header
or a later file modification in case of a given \code{If-Modified-Since} header),
the server does not send the requested file but returns a
\href{https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/304}{304} status
code, indicating, that the data on the requesting device is up-to-date.

Note that if both headers are provided, the \code{If-None-Match} header takes
precedence.

Furthermore, the middleware also supports the headers \code{If-Match}, which
returns the object if the hash matches (it also supports "*" to always return
the file), as well as \code{If-Unmodified-Since}, which returns the object if it
has not been modified since a certain time.
If the conditions are not met, a
\href{https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/412}{412} status
code is returned (Precondition Failed).
See examples below.
}
\examples{
#############################################################################
# setup a static directory with ETag caching

static_dir = file.path(tempdir(), "static")
if (!dir.exists(static_dir)) dir.create(static_dir)

file_path = file.path(static_dir, "example.txt")
writeLines("Hello World", file_path)

# get the time the file was last modified in UTC time
last_modified = as.POSIXlt(file.info(file_path)[["mtime"]], tz = "UTC")
file_hash = digest::digest(file = file_path, algo = "crc32")

time_fmt = "\%a, \%d \%b \%Y \%H:\%M:\%S GMT"



#############################################################################
# setup the Application with the ETag Middleware
app = Application$new()
app$append_middleware(ETagMiddleware$new())
app$add_static(path = "/", static_dir)



#############################################################################
# Example Requests

# Request the file returns the file with ETag headers
req = Request$new(path = "/example.txt")
# note that it also returns the Last-Modified and ETag headers
app$process_request(req)


# provide matching hash of the file in the If-None-Match header to check Etag
# => 304 Not Modified (Can be cached)
req = Request$new(path = "/example.txt",
                  headers = list("If-None-Match" = file_hash))
# note status_code 304 Not Modified
app$process_request(req)


# provide a wrong hash, returns the file normally
req = Request$new(path = "/example.txt",
                  headers = list("If-None-Match" = "WRONG HASH"))
app$process_request(req)


# alternatively, you can provide a timestamp in the If-Modified-Since header
# => 304 Not Modified (Can be cached)
modified_since = format(last_modified + 1, time_fmt)
req = Request$new(path = "/example.txt",
                  headers = list("If-Modified-Since" = modified_since))
app$process_request(req)


# provide both headers: If-None-Match takes precedence
# in this case:
#  - if none match => modified (No cache)
#  - if modified since => NOT MODIFIED (cached)
# => Overall: modified = no cache
modified_since = format(last_modified + 1, time_fmt)
req = Request$new(path = "/example.txt",
                  headers = list("If-None-Match" = "CLEARLY WRONG",
                                 "If-Modified-Since" = modified_since))
app$process_request(req)


# provide matching hash of the file in the If-Match header to check Etag
# => 412 Precondition Failed
req = Request$new(path = "/example.txt",
                  headers = list("If-Match" = "OTHER HASH"))
# note status_code 412 Precondition Failed
app$process_request(req)


# Use If-Unmodified-Since
unmodified_since = format(last_modified - 1, time_fmt)
req = Request$new(path = "/example.txt",
                  headers = list("If-Unmodified-Since" = unmodified_since)
)
# note status_code 412 Precondition Failed
app$process_request(req)



#############################################################################

# use an alternative hash function (use name of the file)
hash_on_filename = function(x) x
# also use an alternate last_modified time function
always_1900 = function(x) as.POSIXlt("1900-01-01 12:34:56", tz = "GMT")


# setup the app again
app = Application$new(middleware = list(
  ETagMiddleware$new(hash_function = hash_on_filename,
                     last_modified_function = always_1900)
))
app$add_static(path = "/", file_path = static_dir)


# test the requests
req = Request$new(path = "/example.txt")
(res = app$process_request(req))

filename = res$body[["file"]]
req = Request$new(path = "/example.txt",
                  headers = list("If-None-Match" = filename))
app$process_request(req)
}
\references{
\href{https://developer.mozilla.org/en/docs/Web/HTTP/Headers/ETag}{MDN}
}
\seealso{
\link{Middleware} \link{Application}
}
\section{Super class}{
\code{\link[RestRserve:Middleware]{RestRserve::Middleware}} -> \code{EtagMiddleware}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{hash_function}}{Function that takes an object or file and computes
the hash of it}

\item{\code{last_modified_function}}{Function that takes an object or file and
computes the last time it was modified}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-EtagMiddleware-new}{\code{ETagMiddleware$new()}}
\item \href{#method-EtagMiddleware-clone}{\code{ETagMiddleware$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-EtagMiddleware-new"></a>}}
\if{latex}{\out{\hypertarget{method-EtagMiddleware-new}{}}}
\subsection{Method \code{new()}}{
Creates ETag middleware object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ETagMiddleware$new(
  routes = "/",
  match = "partial",
  id = "ETagMiddleware",
  hash_function = function(body) {
     if ("file" \%in\% names(body)) {
        
    digest::digest(file = body[["file"]], algo = "crc32")
     }
     else {
        
    digest::digest(body, algo = "crc32")
     }
 },
  last_modified_function = function(body) {
     if ("file" \%in\% names(body)) {
       
     as.POSIXlt(file.info(body[["file"]])[["mtime"]], tz = "GMT")
     }
     else {
    
        as.POSIXlt(Sys.time(), tz = "GMT")
     }
 }
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{routes}}{Routes paths to protect.}

\item{\code{match}}{How routes will be matched: exact or partial (as prefix).}

\item{\code{id}}{Middleware id.}

\item{\code{hash_function}}{a function that generates the ETag hash.
The function takes the body of the response and returns a single
character. Default is crc32 using \link[digest:digest]{digest::digest}.}

\item{\code{last_modified_function}}{a function that takes the body of the
response and returns the last time this was changed. The default is to
take the mtime (last time the file was modified) if its a file,
if the body does not contain a file, the current time is returned (
resulting in no caching)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-EtagMiddleware-clone"></a>}}
\if{latex}{\out{\hypertarget{method-EtagMiddleware-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ETagMiddleware$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
