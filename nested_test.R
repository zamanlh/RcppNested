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

#plot distributions form c and native R - time matched, 100x speed increase!
c_version <- system.time(nodf_null <- replicate(50000, calculateNODF(getRandomMatrix_Fill(csv_mat))$NODF))
r_version <- system.time(nodf_null_2 <- replicate(500, vegan::nestednodf(sortMatrix(getRandomMatrix_Fill(csv_mat)))$statistic["NODF"]))
par(mfcol=c(2,1))
hist(nodf_null)
hist(nodf_null_2)

#different null model generators
nodf_null_col <- replicate(1000, calculateNODF(getRandomMatrix_ColShuffle(ran_mat))$NODF)
nodf_null_row <- replicate(1000, calculateNODF(getRandomMatrix_RowShuffle(ran_mat))$NODF)
nodf_null_fill <- replicate(1000, calculateNODF(getRandomMatrix_Fill(ran_mat))$NODF)
par(mfcol=c(3,1))
hist(nodf_null_col)
hist(nodf_null_row)
hist(nodf_null_fill)

