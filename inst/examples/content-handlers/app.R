#!/usr/bin/env Rscript

library(RestRserve)

app = Application$new(content_type = 'application/json')

app$add_get('/json', function(request, response) {
  response$body = list(answer = 'json')
})

app$add_get('/text', function(request, response) {
  response$content_type = 'text/plain'
  response$body = list(answer = 'text')
})

app$add_get('/unknown-content-type', function(request, response) {
  response$content_type = 'application/x-unknown-content-type'
  # content types which are not registered in ContentHandlers
  # will be encoded as character!
  response$body = serialize("unknown-content-type", NULL)
})

app$add_get('/rds', function(request, response) {
  response$content_type = 'application/rds'
  response$body = serialize(list(answer = 'rds'), NULL)
  # to prevent default `as.character()` encoding for unknown content type
  # we need to provide `identity()` as encode function
  response$encode = identity
})

app$add_get('/rds2', function(request, response) {
  response$content_type = 'application/rds2'
  response$body = serialize(list(answer = 'rds2'), NULL)
})

# Note that new content handler can be registered at any time before application start
ContentHandlers$set_encode('application/rds2', identity)
ContentHandlers$set_encode('application/rds', identity)


app$add_post('/json', function(request, response) {
  response$content_type = 'application/rds'
  response$body = serialize(request$body_decoded, NULL)
})
