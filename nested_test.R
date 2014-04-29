require(Rcpp)

setwd('~/Desktop')
raw_data <- read.csv('interaction-matrix-complexity-30.csv', head=F)
csv_mat <- as.matrix(raw_data)


setwd('~/Dropbox/NODF_R_Ext')
set.seed(10)
sourceCpp("test.cc")

image(nested_matrix(csv_mat))
image(something(csv_mat))

