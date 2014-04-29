#include <Rcpp.h> 
#include <vector>       // std::vector
#include <algorithm>    // std::sort

using namespace Rcpp;

//setup pair type to hold index and value, so we can sort indexes instead of values
typedef std::pair<int,int> myPair;

//sorter for myPair type
bool comparator_r ( const myPair& l, const myPair& r) { 
	return l.first > r.first; 
}


// [[Rcpp::export]]
NumericMatrix sortMatrix(NumericMatrix m) {
	static const int numRows = m.nrow();
	static const int numCols = m.ncol();

	std::vector<myPair> colSums(numCols);
	std::vector<myPair> rowSums(numRows);

	NumericMatrix sortedMatrix(m.nrow(), m.ncol());

	//calculate row and column sums
	for(int rowIdx = 0; rowIdx < numRows; rowIdx++) {
		rowSums[rowIdx].second = rowIdx;
		for(int colIdx = 0; colIdx < numCols; colIdx++) {
			colSums[colIdx].second = colIdx;

			rowSums[rowIdx].first += m(rowIdx,colIdx);
			colSums[colIdx].first += m(rowIdx,colIdx);
		}
	}

	//sort with custom comparator method
	std::sort(rowSums.begin(), rowSums.end(), comparator_r);
	std::sort(colSums.begin(), colSums.end(), comparator_r);

	//create sorted matrix by cross-referencing sorted indexes
	for(int rowIdx = 0; rowIdx < numRows; rowIdx++) {
		for(int colIdx = 0; colIdx < numCols; colIdx++) {
			sortedMatrix(rowIdx, colIdx) = m(rowSums[rowIdx].second, colSums[colIdx].second);
		}
	}

	return sortedMatrix;
}

// [[Rcpp::export]]
List calculateNODF(NumericMatrix m) {
	//Sort Matrix
	NumericMatrix sortedMatrix = sortMatrix(m);

	static const int numRows = m.nrow();
	static const int numCols = m.ncol();

	std::vector<int> colSums(numCols);
	std::vector<int> rowSums(numRows);

	float N_row = 0.0;
	float N_col = 0.0;
	float row_NODF = 0.0;
	float col_NODF = 0.0;
	float final_NODF = 0.0;
	float matrix_fill = 0.0;

	//recalculate row and column sums
	for(int rowIdx = 0; rowIdx < numRows; rowIdx++) {
		for(int colIdx = 0; colIdx < numCols; colIdx++) {
			rowSums[rowIdx] += sortedMatrix(rowIdx,colIdx);
			colSums[colIdx] += sortedMatrix(rowIdx,colIdx);

			matrix_fill += float(sortedMatrix(rowIdx, colIdx))/(numRows*numCols);
		}
	}

	//calculate NODF for rows
	for(int i = 0; i < numRows-1; i++) {
		for(int j=i+1; j < numRows; j++) {
			if(rowSums[j] < rowSums[i]) {
				int k = 0;
				float sum_po_positive = 0.0;
				float sum_po_negative = 0.0;
				
				//N_row = PO for this case
				for(int k=0;k<numCols;k++) {
					if(sortedMatrix(j,k)==1) {
						if(sortedMatrix(i,k)==1) {
							sum_po_positive += 1;
						}
						sum_po_negative +=1;
					}
				} 

				if(sum_po_negative == 0){
					N_row += 1;
				} else {
					N_row += (float(sum_po_positive)/sum_po_negative);
				}
			}
			else {
				N_row += 0;
			}
		}
	}

	//calculate NODF for columns
	for(int i = 0; i < numCols-1; i++) {
		for(int j=i+1; j < numCols; j++) {
			if(colSums[j] < colSums[i]) {
				int k = 0;
				float sum_po_positive = 0.0;
				float sum_po_negative = 0.0;
				
				//N_row = PO for this case
				for(int k=0;k<numRows;k++) {
					if(sortedMatrix(k,j)==1) {
						if(sortedMatrix(k,i)==1) {
							sum_po_positive += 1;
						}
						sum_po_negative +=1;
					}
				} 
				if(sum_po_negative == 0){
					N_col += 1;
				} else {
					N_col += (float(sum_po_positive)/sum_po_negative);
				}
			}
			else {
				N_col += 0;
			}
		}
	}

	//normalize
	row_NODF = 100 * (N_row/(numRows*(numRows-1)/2.0));
	col_NODF = 100 * (N_col/(numCols*(numCols-1)/2.0));
	
	//calculate composite NODF
	final_NODF = ((N_col + N_row)*100)/( (numCols*(numCols-1)/2.0) + (numRows*(numRows-1)/2.0) );
	

	//Return same data as vegan::nestednodf()
	List to_return; 
	to_return["N columns"] = col_NODF;
	to_return["N rows"] = row_NODF;
	to_return["NODF"] = final_NODF;
	to_return["Matrix fill"] = matrix_fill;
	
	return(to_return);
}


// [[Rcpp::export]]
NumericMatrix getRandomMatrix_Fill(NumericMatrix originalMatrix) {
	NumericMatrix random_mat = originalMatrix;
	std::random_shuffle(random_mat.begin(), random_mat.end());
	return random_mat;
}


