CC=clang
CXX=clang++
CXXFLAGS=-O3 -std=c++0x -pg -D_DEBUG -g -c -Wall

all:
	PKG_CXXFLAGS=`Rscript -e 'Rcpp:::CxxFlags()'` \
	PKG_LIBS=`Rscript -e 'Rcpp:::LdFlags()'`  \
	R CMD SHLIB nestedness.cpp
clean:
	rm -rf *.so *.o
