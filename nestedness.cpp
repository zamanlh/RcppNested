#include <Rcpp.h> 
#include <vector>
#include <algorithm>
#include <cassert>
#include <unordered_map>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

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

			bool success = false;
			//for all of this node's neighbors...
			for (std::list<int>::iterator it = m_neighbors.begin(); it != m_neighbors.end(); it++) {
				//go find this ndoe in their neighbor list and remove it
				std::list<int> n_neighbors = n_neighbor_set[*it];

				std::list<int>::iterator itr = n_neighbor_set[*it].begin();
				while (itr != n_neighbor_set[*it].end()) {
					if (*itr == idx) {
						success = true;
						std::list<int>::iterator toErase = itr;
						++itr;
						n_neighbor_set[*it].erase(toErase);
					} else {
						++itr;
					}
				}
			}
			m_neighbor_set.erase(idx);
			return success;
		};

		bool removeN(int idx) {
			std::list<int> n_neighbors = n_neighbor_set[idx];
			bool success = false;

			//for all of this node's neighbors...
			for (std::list<int>::iterator it = n_neighbors.begin(); it != n_neighbors.end(); it++) {
				//go find this ndoe in their neighbor list and remove it
				std::list<int> m_neighbors = m_neighbor_set[*it];

				std::list<int>::iterator itr = m_neighbor_set[*it].begin();
				while (itr != m_neighbor_set[*it].end()) {
					if (*itr == idx) {
						success = true;
						std::list<int>::iterator toErase = itr;
						++itr;
						m_neighbor_set[*it].erase(toErase);
					} else {
						++itr;
					}
				}
			}
			n_neighbor_set.erase(idx);
			return success;
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
			bool success_1 = false;
			bool success_2 = false;
			//remove n from m
			std::list<int>& ms_neighbors = m_neighbor_set[mIdx];
			std::list<int>::iterator itr_m = ms_neighbors.begin();
			while (itr_m != ms_neighbors.end()) {
				if (*itr_m == nIdx) {
					success_1 = true;
					std::list<int>::iterator toErase = itr_m;
					++itr_m;
					ms_neighbors.erase(toErase);
				} else {
					++itr_m;
				}
			}


			std::list<int>& ns_neighbors = n_neighbor_set[nIdx];
			std::list<int>::iterator itr_n = ns_neighbors.begin();
			while (itr_n != ns_neighbors.end()) {
				if (*itr_n == mIdx) {
					success_2 = true;
					std::list<int>::iterator toErase = itr_n;
					++itr_n;
					ns_neighbors.erase(toErase);
				} else {
					++itr_n;
				}
			}

			return success_1 && success_2;
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
NumericMatrix getRandomMatrix_GrowEvents(NumericVector mEvents, NumericVector nEvents, NumericVector edgeEvents) {
	RNGScope scope;
	DynamicBipartiteNet random_net;

	int cur_edges = 0;
	int cur_m = 0;
	int cur_n = 0;

	//make sure event vectors are same length
	//TODO::turn this into an R::error
	assert(mEvents.size() == nEvents.size() == edgeEvents.size());

	for(int t = 0; t < mEvents.size(); t++) {
		int m_event = mEvents[t];
		int n_event = nEvents[t];
		int edge_event = edgeEvents[t];

		int starting_edges = random_net.getEdges().size();

		if(m_event > 0) {
			for(int i=0; i < m_event; i++){
				random_net.addM();	
				cur_m += 1;
			} 
		}
		if(m_event < 0) {
			if(cur_m < -m_event) {
				//std::cerr << "can't have negative m, removing as many as possible" << std::endl;
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
			//std::cerr << "can't have negative n, removing as many as possible" << std::endl;
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
		//lets adjust this to make up for how many edges we got rid of above...
		int edge_adjust = starting_edges - cur_edges;
		edge_event = edge_event + edge_adjust;

		if(cur_edges + edge_event < 0) {
			//std::cerr << "can't have negative edges, removing as many as possible" << std::endl;
			edge_event = -cur_edges;
		}

		if(edge_event > 0) {

			if(cur_m * cur_n - cur_edges <  edge_event) {
				//std::cerr << "can't add that many edges, adding as many as possible" << std::endl;
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
				bool test = random_net.addEdge(non_edge_list[i].first, non_edge_list[i].second);
			}
		}
		if (edge_event < 0) {
			//get edge list, permute, and pick edge_event random ones to remove
			std::vector<std::pair<int, int> > all_edges = random_net.getEdges();
			std::random_shuffle(all_edges.begin(), all_edges.end());
			bool test = false;
			bool test2 = false;
			for(int i=0; i < -edge_event; i++) {
				test = random_net.removeEdge(all_edges[i].first, all_edges[i].second);
				test2 = random_net.hasEdge(all_edges[i].first, all_edges[i].second);

			}


		}
	}

	return random_net.toMatrix();
}


// [[Rcpp::export]]
NumericMatrix getRandomMatrix_GrowMonotonic(NumericMatrix originalMatrix, int timeSteps=100) {
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



// [[Rcpp::export]]
NumericVector getMonotonicTimeseries(NumericMatrix originalMatrix, int timeSteps, int nodfFrequency, bool sortFirst=true) {
	RNGScope scope;
	std::vector<double> nodf_timeseries;

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

	int time_step = 0;

	while(cur_m < num_m || cur_n < num_n || cur_edges < num_edges) {
		if(time_step > 0 && time_step % nodfFrequency == 0) {
			NumericMatrix to_test = random_mat(Range(0, cur_m - 1), Range(0, cur_n - 1));
			if(sortFirst) to_test = sortMatrix(to_test);
			List nodf_result = calculateNODF(to_test);
			nodf_timeseries.push_back(Rcpp::as<double>(nodf_result["NODF"]));
		}
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
	time_step += 1;
	}

	return wrap(nodf_timeseries);
}


// [[Rcpp::export]]
NumericVector getEventTimeseries(NumericVector mEvents, NumericVector nEvents, NumericVector edgeEvents, int nodfFrequency, bool sortFirst=true) {
	RNGScope scope;
	DynamicBipartiteNet random_net;
	std::vector<double> nodf_timeseries;

	int cur_edges = 0;
	int cur_m = 0;
	int cur_n = 0;

	//make sure event vectors are same length
	assert(mEvents.size() == nEvents.size() == edgeEvents.size());

	for(int t = 0; t < mEvents.size(); t++) {
		if(t > 0 && t % nodfFrequency == 0) {
			NumericMatrix to_test = random_net.toMatrix();
			if(sortFirst) to_test = sortMatrix(to_test);
			List nodf_result = calculateNODF(to_test);
			nodf_timeseries.push_back(Rcpp::as<double>(nodf_result["NODF"]));
		}

		int m_event = mEvents[t];
		int n_event = nEvents[t];
		int edge_event = edgeEvents[t];

		int starting_edges = random_net.getEdges().size();

		if(m_event > 0) {
			for(int i=0; i < m_event; i++){
				random_net.addM();	
				cur_m += 1;
			} 
		}
		if(m_event < 0) {
			if(cur_m < -m_event) {
				//std::cerr << "can't have negative m, removing as many as possible" << std::endl;
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
			//std::cerr << "can't have negative n, removing as many as possible" << std::endl;
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
		//lets adjust this to make up for how many edges we got rid of above...
		int edge_adjust = starting_edges - cur_edges;
		edge_event = edge_event + edge_adjust;

		if(cur_edges + edge_event < 0) {
			//std::cerr << "can't have negative edges, removing as many as possible" << std::endl;
			edge_event = -cur_edges;
		}

		if(edge_event > 0) {

			if(cur_m * cur_n - cur_edges <  edge_event) {
				//std::cerr << "can't add that many edges, adding as many as possible" << std::endl;
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
				bool test = random_net.addEdge(non_edge_list[i].first, non_edge_list[i].second);
			}
		}
		if (edge_event < 0) {
			//get edge list, permute, and pick edge_event random ones to remove
			std::vector<std::pair<int, int> > all_edges = random_net.getEdges();
			std::random_shuffle(all_edges.begin(), all_edges.end());
			bool test = false;
			bool test2 = false;
			for(int i=0; i < -edge_event; i++) {
				test = random_net.removeEdge(all_edges[i].first, all_edges[i].second);
				test2 = random_net.hasEdge(all_edges[i].first, all_edges[i].second);

			}


		}
	}

	return wrap(nodf_timeseries);
}

// [[Rcpp::export]]
NumericMatrix getRandomMatrix_AbundanceQuantMatrix(NumericVector mAbundance, NumericVector nAbundance, NumericMatrix quantInteractions, NumericMatrix probInteractions) {
	//TODO make sure all lengths are okay
	NumericVector m_abundance = clone(mAbundance);
	NumericVector n_abundance = clone(nAbundance);

	NumericMatrix to_return(probInteractions.nrow(), probInteractions.ncol());
	double total_interactions = std::accumulate(quantInteractions.begin(), quantInteractions.end(), 0);
	
	double num_cur_interactions = 0;
	double num_m_left = std::accumulate(m_abundance.begin(), m_abundance.end(), 0);
	double num_n_left = std::accumulate(n_abundance.begin(), n_abundance.end(), 0);

	while(num_cur_interactions < total_interactions) {
		//pick an M
		int random_m = round(runif(1, 0, num_m_left - 1)[0]);
		int random_m_idx = -1;

		int m_sum = 0;
		for (int m_idx = 0; m_idx < m_abundance.size(); m_idx++) {
			m_sum += m_abundance[m_idx];
			if (m_sum >= random_m) {
				random_m_idx = m_idx;
				break;
			}
		}

		if (random_m_idx < 0) { 
			//TODO We broke something...
			std::cerr << "nah, this isn't right... m" << std::endl;
		}

		//pick an N
		int random_n = round(runif(1, 0, num_n_left - 1)[0]);
		int random_n_idx = -1;

		int n_sum = 0;
		for (int n_idx = 0; n_idx < n_abundance.size(); n_idx++) {
			n_sum += n_abundance[n_idx];
			if (n_sum >= random_n) {
				random_n_idx = n_idx;
				break;
			}
		}

		if (random_n_idx < 0) { 
			//TODO We broke something...
			std::cerr << "nah, this isn't right... n" << std::endl;
		}
		
		//can they interact? If so, add the edge
		if(runif(1, 0, 1)[0] < probInteractions(random_m_idx, random_n_idx)) {
			//update mAbundances, nAbundances, num_cur_interactions, and num_left
			num_cur_interactions += 1;

			m_abundance[random_m_idx] -= 1;
			n_abundance[random_n_idx] -= 1;

			num_m_left -= 1;
			num_n_left -= 1;

			to_return(random_m_idx, random_n_idx) += 1;
		}
	}
	return to_return;
}



// [[Rcpp::export]]
NumericMatrix getRandomMatrix_Abundance(NumericVector mAbundance, NumericVector nAbundance, int numInteractions, NumericMatrix probInteractions) {
	//TODO make sure all lengths are okay
	NumericVector m_abundance = clone(mAbundance);
	NumericVector n_abundance = clone(nAbundance);

	NumericMatrix to_return(probInteractions.nrow(), probInteractions.ncol());
	double total_interactions = numInteractions;
	
	double num_cur_interactions = 0;
	double num_m_left = std::accumulate(m_abundance.begin(), m_abundance.end(), 0);
	double num_n_left = std::accumulate(n_abundance.begin(), n_abundance.end(), 0);

	int num_tries = 0;

	while(num_cur_interactions < total_interactions) {
		num_tries += 1;

		//if we've tried too many times... we should break out of the loop
		if(num_tries > 100*total_interactions) break;

		//pick an M
		int random_m = round(runif(1, 0, num_m_left - 1)[0]);
		int random_m_idx = -1;

		int m_sum = 0;
		for (int m_idx = 0; m_idx < m_abundance.size(); m_idx++) {
			m_sum += m_abundance[m_idx];
			if (m_sum >= random_m) {
				random_m_idx = m_idx;
				break;
			}
		}

		if (random_m_idx < 0) { 
			//TODO We broke something...
			std::cerr << "nah, this isn't right... m" << std::endl;
		}

		//pick an N
		int random_n = round(runif(1, 0, num_n_left - 1)[0]);
		int random_n_idx = -1;

		int n_sum = 0;
		for (int n_idx = 0; n_idx < n_abundance.size(); n_idx++) {
			n_sum += n_abundance[n_idx];
			if (n_sum >= random_n) {
				random_n_idx = n_idx;
				break;
			}
		}

		if (random_n_idx < 0) { 
			//TODO We broke something...
			std::cerr << "nah, this isn't right... n" << std::endl;
		}
		
		//can they interact? If so, add the edge
		if(runif(1, 0, 1)[0] < probInteractions(random_m_idx, random_n_idx)) {
			//update mAbundances, nAbundances, num_cur_interactions, and num_left
			num_cur_interactions += 1;

			m_abundance[random_m_idx] -= 1;
			n_abundance[random_n_idx] -= 1;

			num_m_left -= 1;
			num_n_left -= 1;

			to_return(random_m_idx, random_n_idx) += 1;
		}
	}
	return to_return;
}
