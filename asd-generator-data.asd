#|
  This file is a part of syntactic-optimization project.

This is a [asd-generator](https://github.com/phoe/asd-generator/) config file.
Run below for auto-regenerating the asd file:

$ ros install phoe/asd-generator
$ update-asdf

  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  provides a few compiler macros for syntactic optimizations e.g. (log (/ x 3) 3) -> (1- (log x 3))

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#


((:package)
 (:rest))

