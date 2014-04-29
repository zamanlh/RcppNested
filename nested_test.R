require(Rcpp)

raw_data <- read.csv('interaction_matrix.csv', head=F)
csv_mat <- as.matrix(raw_data)

sourceCpp("nestedness.cpp")
image(nested_matrix(csv_mat))
image(something(csv_mat))