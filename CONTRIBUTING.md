# Rserve resources

[Rserve](https://github.com/s-u/Rserve/) is amazing software highly undervalued by the community. Here we keep links to useful resources about Rserve:

1. Official Rserve page on Rforge - [news section](http://rforge.net/Rserve/news.html)
1. Description of the **configuration** on [github wiki](https://github.com/s-u/Rserve/wiki/rserve.conf) - **not complete**. For most detailed information it worths to check source code - [setConfig function](https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/Rserv.c#L1094) in `Rserve.c`
1. **http** interface:
    * **request** object format. Rserve passes 4 values (`url`, `query`, `body`, `headers`) to the R code. Parsing logic is described in [FastRWeb::.http.request](https://github.com/s-u/FastRWeb/blob/aaf8847f11903675b1ec7eb9c0e1cc98b92512e5/R/run.R#L58) and [RestRserve:::parse_request](https://github.com/dselivanov/RestRserve/blob/4aecbfb18b8403908c727fa478d161247d591764/R/request.R#L4) functions
    * **response** object from R code defined in [http.c](https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L353-L372) function inside `Rserve.c`
1. Some discissions at issue tracker
    * [RServe for real-time](https://github.com/s-u/Rserve/issues/64)
1. [Rserve tag](https://stackoverflow.com/questions/tagged/rserve) on StackOverflow
