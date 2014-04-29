require(Rcpp)
setwd('~/RcppNested')

raw_data <- read.csv('interaction_matrix.csv', head=F)
csv_mat <- as.matrix(raw_data)

sourceCpp("nestedness.cpp")

vegan::nestednodf(csv_mat)
calculateNODF(csv_mat)



ran_mat <- getRandomMatrix_Fill(csv_mat)
calculateNODF(ran_mat)
vegan::nestednodf(ran_mat)


nodf_null <- replicate(10, calculateNODF(getRandomMatrix_Fill(csv_mat))$NODF)
nodf_null_2 <- replicate(10, vegan::nestednodf(getRandomMatrix_Fill(csv_mat))$statistic["NODF"])


par(mfcol=c(2,1))
hist(nodf_null)
hist(nodf_null_2)






require(Rcpp)
setwd('~/RcppNested')
sourceCpp("nestedness.cpp")
m <- matrix(c(1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,0,0,1,0,1,0,0,1,0,0,0,0), nrow=5)
calculateNODF(m)
