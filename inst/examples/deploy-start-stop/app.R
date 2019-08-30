library(RestRserve)

RestRserveApp = RestRserveApplication$new()

RestRserveApp$add_get('/hello', function(request, response) {
  response$body = 'Hello from RestRserve!'
})
