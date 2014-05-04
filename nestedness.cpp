#include <Rcpp.h> 
#include <vector>
#include <algorithm>
#include <cassert>
#include <unordered_map>
using namespace Rcpp;

//setup pair type to hold index and value, so we can sort indexes instead of values
typedef std::pair<int,int> myPair;

//sorter for myPair type
static bool comparator_r ( const myPair& l, const myPair& r) { 
	return l.first > r.first; 
}

class DynamicBipartiteNet 
{
	private:
		std::unordered_map<int, std::list<int> > m_neighbor_set;
		std::unordered_map<int, std::list<int> > n_neighbor_set;
		int num_n;
		int num_m;
	public:
		~DynamicBipartiteNet() {
			m_neighbor_set.clear();
			n_neighbor_set.clear();
			m_neighbor_set.~unordered_map<int, std::list<int> >();
			n_neighbor_set.~unordered_map<int, std::list<int> >();
		};
		DynamicBipartiteNet() {
			num_n = 0;
			num_m = 0;
		};

		std::vector<int> getNs() {
			std::vector<int> n_keys;
			std::unordered_map<int, std::list<int> >::iterator iter;
			for(iter = n_neighbor_set.begin(); iter != n_neighbor_set.end(); iter++) {
				n_keys.push_back(iter->first);
			}
			return n_keys;
		};

		std::vector<int> getMs() {
			std::vector<int> m_keys;
			std::unordered_map<int, std::list<int> >::iterator iter;
			for(iter = m_neighbor_set.begin(); iter != m_neighbor_set.end(); iter++) {
				m_keys.push_back(iter->first);
			}
			return m_keys;
		};

		std::vector<std::pair<int, int> > getEdges() {
			std::vector<std::pair<int, int> > edge_pairs;
			std::unordered_map<int, std::list<int> >::iterator m_iter;
			std::list<int>::iterator n_iter;
			for(m_iter = m_neighbor_set.begin(); m_iter != m_neighbor_set.end(); m_iter++) {
				int m_idx = m_iter->first;
				for(n_iter = (m_iter->second).begin(); n_iter != (m_iter->second).end(); n_iter++) {
					int n_idx = *n_iter;
					edge_pairs.push_back( std::pair<int, int>(m_idx, n_idx) );
				}

			}
			return edge_pairs;

		};

		int addN() {
			num_n += 1;
			n_neighbor_set[num_n] = std::list<int>();
			return num_n;
		};

		int addM() {
			num_m += 1;
			m_neighbor_set[num_m] = std::list<int>();
			return num_m;
		};

		bool removeM(int idx) {
			std::list<int> m_neighbors = m_neighbor_set[idx];

			//for all of this node's neighbors...
			for (std::list<int>::iterator it = m_neighbors.begin(); it != m_neighbors.end(); it++) {
				//go find this ndoe in their neighbor list and remove it
				std::list<int> n_neighbors = n_neighbor_set[*it];

				std::list<int>::iterator itr = n_neighbor_set[*it].begin();
				while (itr != n_neighbor_set[*it].end()) {
					if (*itr == idx) {
						std::list<int>::iterator toErase = itr;
						++itr;
						n_neighbor_set[*it].erase(toErase);
					} else {
						++itr;
					}
				}
			}
			m_neighbor_set.erase(idx);
		};

		bool removeN(int idx) {
			std::list<int> n_neighbors = n_neighbor_set[idx];

			//for all of this node's neighbors...
			for (std::list<int>::iterator it = n_neighbors.begin(); it != n_neighbors.end(); it++) {
				//go find this ndoe in their neighbor list and remove it
				std::list<int> m_neighbors = m_neighbor_set[*it];

				std::list<int>::iterator itr = m_neighbor_set[*it].begin();
				while (itr != m_neighbor_set[*it].end()) {
					if (*itr == idx) {
						std::list<int>::iterator toErase = itr;
						++itr;
						m_neighbor_set[*it].erase(toErase);
					} else {
						++itr;
					}
				}
			}
			n_neighbor_set.erase(idx);
		};

		bool addEdge(int mIdx, int nIdx) {
			if(&m_neighbor_set[mIdx] == NULL || &n_neighbor_set[nIdx] == NULL) return false;

			m_neighbor_set[mIdx].push_back(nIdx);
			n_neighbor_set[nIdx].push_back(mIdx);
			return true;
		};

		bool hasEdge(int mIdx, int nIdx) {
			std::list<int> m_neighbors = m_neighbor_set[mIdx];
			bool has_edge = false;

			for (std::list<int>::iterator it = m_neighbors.begin(); it != m_neighbors.end(); it++) {
				if(*it == nIdx) has_edge=true;
			}

			return has_edge;

		};

		bool removeEdge(int mIdx, int nIdx) {
			//remove n from m
			std::list<int> m_neighbors = m_neighbor_set[mIdx];

			for (std::list<int>::iterator it = m_neighbors.begin(); it != m_neighbors.end(); it++) {
				//go find this node in their neighbors list and remove it
				std::list<int> n_neighbors = n_neighbor_set[*it];
				for (std::list<int>::iterator n_it = n_neighbors.begin(); n_it != n_neighbors.end(); n_it++) {
					if(*n_it == mIdx) n_it = n_neighbors.erase(n_it);
				}
			}

			std::list<int> n_neighbors = n_neighbor_set[nIdx];
			for (std::list<int>::iterator it = n_neighbors.begin(); it != n_neighbors.end(); it++) {
				//go find this ndoe in their neighbor list and remove it
				std::list<int> m_neighbors = m_neighbor_set[*it];
				for (std::list<int>::iterator m_it = m_neighbors.begin(); m_it != m_neighbors.end(); m_it++) {
					if(*m_it == nIdx) m_it = m_neighbors.erase(m_it);
				}
			}
		}

		NumericMatrix toMatrix() {

			std::vector<int> all_m = getMs();
			std::vector<int> all_n = getNs();

			NumericMatrix mat(all_m.size(), all_n.size());

			for(int i=0; i < all_m.size(); i++) {
				for(int j=0; j < all_n.size(); j++) {
					if (hasEdge(all_m[i], all_n[j])) mat(i,j) = 1;
				}
			}

			return mat;
		};


};

// [[Rcpp::export]]
NumericMatrix sortMatrix(NumericMatrix bipartiteAdjMatrix) {
	int numRows = bipartiteAdjMatrix.nrow();
	int numCols = bipartiteAdjMatrix.ncol();

	std::vector<myPair> colSums(numCols);
	std::vector<myPair> rowSums(numRows);

	NumericMatrix sortedMatrix(bipartiteAdjMatrix.nrow(), bipartiteAdjMatrix.ncol());

	//calculate row and column sums
	for(int rowIdx = 0; rowIdx < numRows; rowIdx++) {
		rowSums[rowIdx].second = rowIdx;
		for(int colIdx = 0; colIdx < numCols; colIdx++) {
			colSums[colIdx].second = colIdx;

			rowSums[rowIdx].first += bipartiteAdjMatrix(rowIdx,colIdx);
			colSums[colIdx].first += bipartiteAdjMatrix(rowIdx,colIdx);
		}
	}

	//sort with custom comparator method
	std::sort(rowSums.begin(), rowSums.end(), comparator_r);
	std::sort(colSums.begin(), colSums.end(), comparator_r);

	//create sorted matrix by cross-referencing sorted indexes
	for(int rowIdx = 0; rowIdx < numRows; rowIdx++) {
		for(int colIdx = 0; colIdx < numCols; colIdx++) {
			sortedMatrix(rowIdx, colIdx) = bipartiteAdjMatrix(rowSums[rowIdx].second, colSums[colIdx].second);
		}
	}

	return sortedMatrix;
}

// [[Rcpp::export]]
List calculateNODF(NumericMatrix bipartiteAdjMatrix) {

	int numRows = bipartiteAdjMatrix.nrow();
	int numCols = bipartiteAdjMatrix.ncol();

	std::vector<int> colSums(numCols);
	std::vector<int> rowSums(numRows);
	
	float N_row = 0.0;
	float N_col = 0.0;
	float row_NODF = 0.0;
	float col_NODF = 0.0;
	float final_NODF = 0.0;
	double matrix_fill = 0.0;

	//recalculate row and column sums
	for(int rowIdx = 0; rowIdx < numRows; rowIdx++) {
		for(int colIdx = 0; colIdx < numCols; colIdx++) {
			rowSums[rowIdx] += bipartiteAdjMatrix(rowIdx,colIdx);
			colSums[colIdx] += bipartiteAdjMatrix(rowIdx,colIdx);
			//std::cout << "matrix_fill: " << matrix_fill << std::endl;
			matrix_fill += double(bipartiteAdjMatrix(rowIdx, colIdx))/double(numRows*numCols);
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
					if(bipartiteAdjMatrix(j,k)==1) {
						if(bipartiteAdjMatrix(i,k)==1) {
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
					if(bipartiteAdjMatrix(k,j)==1) {
						if(bipartiteAdjMatrix(k,i)==1) {
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
NumericMatrix getRandomMatrix_GrowEvents(NumericMatrix originalMatrix, NumericVector mEvents, NumericVector nEvents, NumericVector edgeEvents) {
	RNGScope scope;

	DynamicBipartiteNet random_net;
	//NumericMatrix random_mat(originalMatrix.nrow() * 2, originalMatrix.ncol() * 2);

	int cur_edges = 0;
	int cur_m = 0;
	int cur_n = 0;

	//make sure event vectors are same length
	assert(mEvents.size() == nEvents.size() == edgeEvents.size());

	for(int t = 0; t < mEvents.size(); t++) {
		int m_event = mEvents[t];
		int n_event = nEvents[t];
		int edge_event = edgeEvents[t];

		if(m_event > 0) {
			for(int i=0; i < m_event; i++){
				random_net.addM();	
				cur_m += 1;
			} 
		}
		if(m_event < 0) {
			if(cur_m < -m_event) {
				std::cerr << "can't have negative m, removing as many as possible" << std::endl;
				m_event = -cur_m;
			}
			//get Ms and pick a random one to remove
			std::vector<int> allM = random_net.getMs();
			std::random_shuffle(allM.begin(), allM.end());
			
			for(int i=0; i < -m_event; i++) {
				random_net.removeM(allM[i]);
				cur_m -= 1;
			}
		}

		
		if(cur_n + n_event < 0) {
			std::cerr << "can't have negative n, removing as many as possible" << std::endl;
			n_event = -cur_n;

		}

		if(n_event > 0) {
			for(int i=0; i<n_event; i++) {
				random_net.addN();
				cur_n += 1;
			} 
		}
		if(n_event < 0) {
			//get Ns and pick a random one to remove
			//get Ms and pick a random one to remove
			std::vector<int> allN = random_net.getNs();
			std::random_shuffle(allN.begin(), allN.end());
			for(int i=0; i < -n_event; i++) {
				random_net.removeN(allN[i]);
				cur_n -= 1;
			};

		}

		cur_edges = random_net.getEdges().size();
		
		if(cur_edges + edge_event < 0) {
			std::cerr << "can't have negative edges, removing as many as possible" << std::endl;
			edge_event = -cur_edges;
		}

		if(edge_event > 0) {
			if(cur_m * cur_n - cur_edges <  edge_event) {
				std::cerr << "can't add that many edges, adding as many as possible" << std::endl;
				edge_event = cur_m * cur_n - cur_edges;
			}
			std::vector<int> allM = random_net.getMs();
			std::vector<int> allN = random_net.getNs();
			
			std::vector<std::pair<int, int> > non_edge_list;

			//make a list of non-edges in network: O(N*M*deg(M))
			for(int i=0; i < allM.size(); i++) {
				for(int j=0; j < allN.size(); j++) {
					if(random_net.hasEdge(allM[i], allN[j]) == false) {
						//add pair to "non-edge list"
						non_edge_list.push_back(std::pair<int, int> (allM[i], allN[j]));
					}
				}
			}

			//permute non-edge list and pick edge_event from them to add
			std::random_shuffle(non_edge_list.begin(), non_edge_list.end());
			for(int i=0; i < edge_event; i++) {
				bool testing = random_net.addEdge(non_edge_list[i].first, non_edge_list[i].second);
				cur_edges += 1;
			}
		}
		if (edge_event < 0) {
			//get edge list, permute, and pick edge_event random ones to remove
			std::vector<std::pair<int, int> > all_edges = random_net.getEdges();
			std::random_shuffle(all_edges.begin(), all_edges.end());

			for(int i=0; i < -edge_event; i++) {
				random_net.removeEdge(all_edges[i].first, all_edges[i].second);
				cur_edges -= 1;
			}
		}
	}

	return random_net.toMatrix();
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

	int cur_edges = 1;
	int cur_m = 1;
	int cur_n = 1;

	while(cur_m < num_m || cur_n < num_n || cur_edges < num_edges) {
		//add m (hosts)
		if (cur_m < num_m) cur_m += rpois(1, lambda_m)[0];
		cur_m = std::min(cur_m, num_m);

		//add n (parasites)
		if (cur_n < num_n) cur_n += rpois(1, lambda_n)[0];
		cur_n = std::min(cur_n, num_n);

		//TODO: make sure cur_n and cur_m never go > m and n, since in this model,
		//we have a max size on our matrix from the start. 

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

