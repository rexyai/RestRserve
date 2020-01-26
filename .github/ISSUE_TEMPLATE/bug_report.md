---
name: Bug report
about: Create a report to help us improve RestRserve
title: "[BUG] short bug description "
labels: bug, not-confirmed
assignees: ''

---

### Reporting an Issue
Make use of the *Preview* tab just above!

### Before filing an issue
Please ensure that you:
- searched existing [GitHub issues](https://github.com/rexyai/RestRserve/issues) which can be searched among open and closed ones;
- read the [Contributing](https://github.com/rexyai/RestRserve/blob/master/CONTRIBUTING.md) page for details on preferred reporting and style;

### Describe the bug
A clear and concise description of what the bug is. 

### To Reproduce
Please provide a **minimal** example. 'minimal' generally means that example should not use any packages except `RestRserve`. If you are not sure what 'minimal reproducible example' is, please consult [here](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example). **We appreciate your time to make a bug report.**

For example:
```r
library("RestRserve")
app = Application$new()
app$add_post(path = "/bug", function(req, res) {
  res$set_body(req$body)
})
app$logger$set_log_level("debug")
request = Request$new(
  path = "/bug", 
  method = "POST", 
  body = '{"key":invalid_json}', 
  content_type = "application/json"
)

response = app$process_request(request)
cat(response$body)
```

### Expected behavior
A clear and concise description of what you expected to happen.

### Environment information
Please provide output of the `sessionInfo()` command. 

For example:
```
R version 3.6.0 (2019-04-26)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] RestRserve_0.2.0

loaded via a namespace (and not attached):
[1] compiler_3.6.0  backports_1.1.5 R6_2.4.1        tools_3.6.0     Rcpp_1.0.3      uuid_0.1-2      checkmate_1.9.4 jsonlite_1.6    mime_0.7 
```

### Additional context
Add any other context about the problem here.
