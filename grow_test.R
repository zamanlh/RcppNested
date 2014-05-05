require(Rcpp)
setwd('~/RcppNested')
sourceCpp("nestedness.cpp")

d <- read.csv('~/Desktop/200up_events.csv')
d <- subset(d, rep==1 & update < 50000)
summary(d)

original_mat <- as.matrix(read.csv('interaction_matrix.csv', head=F))
calculateNODF(sortMatrix(original_mat))


getEventTimeseries(d$host_event, d$parasite_event, d$edge_event)
t_s <- replicate(10, calculateNODF(sortMatrix(getRandomMatrix_GrowEvents(d$host_event, d$parasite_event, d$edge_event)))$NODF)
t_rw <- replicate(10, calculateNODF(sortMatrix(getRandomMatrix_GrowMonotonic(original_mat, length(d$host_event))))$NODF)


image(sortMatrix(t))




#different null model generators
nodf_null_col <- replicate(1000, calculateNODF(getRandomMatrix_ColShuffle(t))$NODF)
nodf_null_row <- replicate(1000, calculateNODF(getRandomMatrix_RowShuffle(t))$NODF)
nodf_null_fill <- replicate(1000, calculateNODF(getRandomMatrix_Fill(t))$NODF)
par(mfcol=c(3,1))
hist(nodf_null_col)
hist(nodf_null_row)
hist(nodf_null_fill)
