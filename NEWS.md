## Changelog

* 2019-06-25
    * Start of the new major **version 0.2**
      - much more straightforward process of the error handling - thanks to structured errors introduced in R 3.6.0
      - so we depend on R 3.6.0 now
    * **Breaking change** removed `interrupt`(see above - we use R's standard structured errors). See new `raise()` function (wrapper around `stop()` and `errorCondition()`).
    * **renamed** `BearerAuthBackend` -> `AuthBackendBearer`, `BasicAuthBackend` -> `AuthBackendBasic`. Provide deprecation warning.
    * usage of `forward()` function is **deprecated**. Handlers and middleware just need to modify `request`/`response` objects or throw an exception using `raise()` function.
    * **New**. We provide `HTTPErrorFactory` class generator to facilitate creation of the standard errors to be used with `raise()` function.
    * **Breaking change** default `content_type` in `RestRserveApplication` and `RestRserveResponse` has changed from `application/json` to `text/plain`.
* 2018-12-29
    * **Breaking change** made `logger` field public in `RestRserveApplication` and removed it from constructor. Hopefully won't hurt too many people.
    * fixed #24
* 2018-11-22
    * Call response middleware only if corresponding request middleware was called before.
* 2018-11-20
    * **Breaking change** - refactor middleware. Now it has to return `forward()` in order to continue execution or `interrupt()` to not continue execution (for example when authorization is failed).
