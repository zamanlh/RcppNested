To build, modify the Makefile for your system and run "make". Currently this is setup to build on Mac with llvm. This requires Rcpp.

The nested_test.R runs this NODF function and the one contained in the vegan R package. 

This is under heavy development, and is highly experimental at the moment. In the future, I plan to release this as an R package. 

Some preliminary stats on the vegan R package compared with the new NODF function in C

> c_version 
user  system elapsed 
0.240   0.001   0.240 
> r_version
user  system elapsed 
23.141   0.113  23.048 