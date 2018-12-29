## Changelog

* 2018-12-29
    * **Breaking change** made `logger` field public in `RestRserveApplication` and removed it from constructor. Hopefully won't hurt too many people. 
    * fixed #24
* 2018-11-22
    * Call reponse middleware only if corresponding request middleware was called before.
* 2018-11-20
    * **Breaking change** - refactor middleware. Now it has to return `forward()` in order to continue execution or `interrupt()` to not continue execution (for example when authorization is failed).
