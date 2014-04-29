require(Rcpp)
setwd('~/RcppNested')

raw_data <- read.csv('interaction_matrix.csv', head=F)
csv_mat <- as.matrix(raw_data)

sourceCpp("nestedness.cpp")

vegan::nestednodf(csv_mat)
calculateNODF(csv_mat)