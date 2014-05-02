#include <Rcpp.h> 
#include <vector>
#include <algorithm>

using namespace Rcpp;

//setup pair type to hold index and value, so we can sort indexes instead of values
typedef std::pair<int,int> myPair;

//sorter for myPair type
static bool comparator_r ( const myPair& l, const myPair& r) { 
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
	for(int i = 0; i < numRows - 1; i++) {
		for(int j=i+1; j < numRows; j++) {
			if(rowSums[j] < rowSums[i]) {
				int k = 0;
				float sum_po_positive = 0.0;
				float sum_po_negative = 0.00001;
				
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
	for(int i = 0; i < numCols - 1; i++) {
		for(int j=i+1; j < numCols; j++) {
			if(colSums[j] < colSums[i]) {
				int k = 0;
				float sum_po_positive = 0.0;
				float sum_po_negative = 0.00001;
				
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
	RNGScope scope;
	NumericMatrix random_mat = clone(originalMatrix);
	std::random_shuffle(random_mat.begin(), random_mat.end());
	return random_mat;
}
// [[Rcpp::export]]
NumericMatrix getRandomMatrix_RowShuffle(NumericMatrix originalMatrix) {
	RNGScope scope;
	NumericMatrix random_mat = clone(originalMatrix);

	for(int i = 0; i < random_mat.nrow(); i++) {
		NumericMatrix::Row single_row = random_mat(i,_);
		std::random_shuffle(single_row.begin(), single_row.end());
	}

	return random_mat;
}

// [[Rcpp::export]]
NumericMatrix getRandomMatrix_ColShuffle(NumericMatrix originalMatrix) {
	RNGScope scope;
	NumericMatrix random_mat = clone(originalMatrix);

	for(int i = 0; i < random_mat.ncol(); i++) {
		NumericMatrix::Column single_column = random_mat(_,i);
		std::random_shuffle(single_column.begin(), single_column.end());
	}

	return random_mat;
}

// [[Rcpp::export]]
NumericMatrix getRandomMatrix_GrowMonotonic(NumericMatrix originalMatrix, int timeSteps) {
	RNGScope scope;
	NumericMatrix random_mat(originalMatrix.nrow(), originalMatrix.ncol());
	int num_edges = std::accumulate(originalMatrix.begin(), originalMatrix.end(), 0);
	int num_m = originalMatrix.nrow();
	int num_n = originalMatrix.ncol();

	double lambda_edge = (double(num_edges) - 1) /timeSteps;
	double lambda_m = (double(num_m) - 1)/timeSteps;
	double lambda_n = (double(num_n) - 1)/timeSteps;

	NumericVector edge_event_vector = rpois(timeSteps*3, lambda_edge);
	NumericVector m_event_vector = rpois(timeSteps, lambda_m);
	NumericVector n_event_vector = rpois(timeSteps, lambda_n);

	int cur_edges = 1;
	int cur_m = 1;
	int cur_n = 1;

	while(cur_m < num_m || cur_n < num_n || cur_edges < num_edges) {
		//add m (hosts)
		if (cur_m < num_m) cur_m += rpois(1, lambda_m)[0];

		//add n (parasites)
		if (cur_n < num_n) cur_n += rpois(1, lambda_n)[0];

		//add edges 
		if(cur_edges < num_edges) {
			int edge_event = rpois(1, lambda_edge)[0];

			if(cur_m*cur_n - cur_edges < edge_event) {
				//not enough space for edges...
				//max out how many edges we should add then
				edge_event = cur_m * cur_n - cur_edges;
			}

			int num_edges_left = edge_event;

			while(num_edges_left > 0) {
				//better algorithm : count how many non-edges there are, use that as prob of picking 1 of them
				//iterate through matrix until we find a 0, then pull from ^ prob to add edge or not...			
				int num_vacant_edges = cur_m * cur_n - cur_edges;
				double prob_add_edge = 1 / double(num_vacant_edges);

				for(int i = 0; i < cur_m; i++) {
					for(int j=0; j < cur_n; j++) {
						double ran_draw = runif(1, 0, 1)[0];

						if(random_mat(i,j) == 0 && ran_draw < prob_add_edge && num_edges_left > 0) {
							num_edges_left -= 1;
							cur_edges += 1;
							random_mat(i, j) = 1;
						}
					}
				}
			}
		}
	}

	return random_mat;
}

