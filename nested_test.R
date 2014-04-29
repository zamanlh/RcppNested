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

c_version <- system.time(nodf_null <- replicate(1000, calculateNODF(getRandomMatrix_Fill(csv_mat))$NODF))
r_version <- system.time(nodf_null_2 <- replicate(1000, vegan::nestednodf(sortMatrix(getRandomMatrix_Fill(csv_mat)))$statistic["NODF"]))

par(mfcol=c(2,1))
hist(nodf_null)
hist(nodf_null_2)
