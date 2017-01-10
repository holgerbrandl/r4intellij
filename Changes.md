Version History of R4Intellij

## v0.11


* New R icon

Parser improvements

* fixed: parser fails to work for higher dimensional arrays #53
* fixed: functions declared with = don't show up in structure view #72
* allow for leading commas in functions invokations (e.g. `myFun(,5)`) which seem bad practice (use named arg instead!) but are common

