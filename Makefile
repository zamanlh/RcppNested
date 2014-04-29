CC=clang
CXX=clang++

all:
	PKG_CXXFLAGS=`Rscript -e 'Rcpp:::CxxFlags()'` \
	PKG_LIBS=`Rscript -e 'Rcpp:::LdFlags()'`  \
	R CMD SHLIB nestedness.cpp