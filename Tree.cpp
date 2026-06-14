//Tree.cpp. Reminder - this tree is simple, connected, and directed. It has set properties.
//By Lorenz Hunter
//Spring 2025

#include "Tree.h"
#include <queue>
#include <iostream>
#include "math.h"

using std::queue;
using std::cout;

//Note: the branching factor is set to 3 by default.
Tree::Tree() {
	this->root_ = NULL;
	this->set_branching_factor(3);
}
Tree::Tree(const Tree& other) {
	this->root_ = NULL;
	this->set_branching_factor(other.branching_factor_);
	this->set_size(other.size_);
	this->root_ = other.root_;
	this->alc_unq_ = other.alc_unq_;
	this->unq_ = other.unq_;
}
Tree::~Tree() {
	this->root_ = NULL;
}

//Note: Tree::operator= is fully operational 
Tree& Tree::operator=(Tree& other) {
	vector<double> array; array.reserve(other.get_size());
	int index = 0;
	if (other.root_ == this->root_) {
		return *this;
	} else if (other.root_ == NULL) {
		this->root_ = NULL;
		return *this;
	} else {
		array = other.convert();
		for (auto& index : array) {
			this->insert(index);
		}
		return *this;
	}
}

//functions

//Insert a data into the tree. Will insert to the first void space encountered via use of DFS in a breadth-first, iterative manner.
void Tree::insert(double data) {
	//root case
	if (this->root_ == NULL) {
		pair<double, int> new_pair (data, this->unq_);
		this->root_ = new TreeNode(new_pair);
		this->increment_unq();
		this->increment_size();
		this->increment_alc_unq();
		return;
	}
	//DFS algorithm here.
	const int orig_unq = this->unq_;
	int bf = this->branching_factor_;
	//calculate ch
	int ch;
	int a = 0;
	for (int abs_ind = 0; abs_ind < this->unq_; abs_ind++) {
		pair<double, int> data = DFS(abs_ind);
		if (get<1>(data) == -1) {
			a++;
			ch = this->height(a);
			break;
		}
		if (abs_ind == this->unq_-1) {
			a++;
			ch = this->height();
			break;
		}

		a++;
	}	
	int dh;
	if (this->is_balanced(ch)) {
		dh = ch + 1;
		ch = ch + 1;
	} else if (!this->is_balanced(ch)) {
		dh = ch;
	}	
	const int desired_depth = dh;	

	const int d_abs_ind = cap_less_one(dh) + (a - cap_less_one(dh));
	
	//calculate i
	vector<int> D; 
	for (int n = cap_less_one(dh); n < cap(dh); n++) {
		D.push_back(n);
	}
	vector<int> C;
	for (int n = cap_less_one(dh); n < d_abs_ind; n++) {
		C.push_back(n);
	}
	int x = 0;
	int n_ssts = 0;
	for (auto& n : C) {
		vector<int>::iterator it = find (D.begin(), D.end(), n);
		if (it != D.end()) {
			x++;
		}
		if (x == bf) { 
			x = 0;
			n_ssts++;
		}
	}
	n_ssts += 1;
	int i = n_ssts;
	//create array = A, "path"
	vector<int> A; A.reserve(dh+1);
	A.assign(dh+1, 0);

	int abv = -1;
	for (int index = dh; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = d_abs_ind;
			if (ch == 1) {
				continue;
			} else if (ch == 2) {
				abv = i + (cap_less_one(ch-1) - 1);
			} else {
				abv = i + (cap_less_one(ch-1) - 1);
			} 
		} else {
			A.at(index) = abv;
			if (ch == 1) {
				continue;
			} else if (ch == 2) {
				abv = ( (abv - cap_less_one(ch)) / bf) + 1;
			} else {
				abv = ( (abv - cap_less_one(ch)) / bf ) + cap_less_one(ch-1) + 1;			
			}
		}
		ch--;
	}


	//Traverse A
	TreeNode* prev = this->root_;
	for (vector<int>::iterator it = A.begin() + 1; it != A.end() - 1; it++) {
		int traversal_index = (*(it) - 1) % bf;
		prev = prev->subtrees_.at(traversal_index);
	}

	//insert
	int insertion_index;
	if (d_abs_ind == this->unq_ - 1) {
		insertion_index = (d_abs_ind + 1 - 1) % bf;
	} else {
		insertion_index = (d_abs_ind - 1) % bf;
	}
	pair<double, int> new_pair (data, orig_unq);

	if (prev->subtrees_.capacity() == 0) {
		for (int index = 0; index < bf; index++) {
			pair<double, int> new_pair (0, -1);
			TreeNode* new_node = new TreeNode(new_pair);
			prev->subtrees_.insert(prev->subtrees_.begin(), new_node);
		}
	}
	prev->subtrees_.at(insertion_index) = new TreeNode(new_pair);
	//calculate new alc_unq_
	this->sort();
	return;
}


//Print the tree in a line
void Tree::print() {
	if (this->root_ == NULL) {
		return;
	} else {
		print_helper();
		cout << "\n";
	}		
}

void Tree::print_helper() {
	for (int node = 0; node < this->unq_; node++) {
		pair<double, int> current = DFS(node);
		if (get<0>(current) != 0) {
			cout << get<0>(current) << " ";
		}
	}
}

//Note: after each insertion or removal, the tree must be sorted with respect to abs-index.
//Note: We are simply substituting "d_abs_ind" with "this->unq_ - 1", requiring the tree to be sorted.
void Tree::remove() {
	//root case
	if (this->root_ == NULL) {
		return;
	}
	//DFS algorithm here.
	int bf = this->branching_factor_;
	//calculate ch
	int ch = this->height();
	int dh;
	if (this->is_balanced()) {
		dh = ch + 1;
	} else if (!this->is_balanced()) {
		dh = ch;
	}	
	if (this->cap_less_one(dh) == -1) {
		cout << "Error: in Tree::remove(): Tree::cap_less_one(dh) returns -1\n";
		return;
	}
	//calculate i
	vector<int> D; 
	for (int n = cap_less_one(dh); n < cap(dh); n++) {
		D.push_back(n);
	}
	vector<int> C;
	for (int n = cap_less_one(dh); n < this->unq_ - 1; n++) {
		C.push_back(n);
	}
	int x = 0;
	int n_ssts = 0;
	for (auto& n : C) {
		vector<int>::iterator it = find (D.begin(), D.end(), n);
		if (it != D.end()) {
			x++;
		}
		if (x == bf) {
			x = 0;
			n_ssts++;
		}
	}
	n_ssts += 1;
	int i = n_ssts;
	//create array = A, "path"
	vector<int> A; A.reserve(dh+1);
	A.assign(dh+1, 0);
	A.at(dh) = this->unq_ - 1;
	
	int abv = -1;
	for (int index = dh; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = this->unq_ - 1;
			if (ch == 1) {
				continue;
			} else {
				abv = i + (cap_less_one(ch-1) - 1);
			}
		} else {
			A.at(index) = abv;
			if (ch == 1) {
				continue;
			} else if (ch == 2) {
				abv = ( (abv - cap_less_one(ch)) / bf) + 1;
			} else {
				abv = ( (abv - cap_less_one(ch)) / bf ) + cap_less_one(ch-1) + 1;			
			} 
		}
		ch--;
	}

	//calculate desired_d and desired_depth
	int desired_depth = this->height(this->unq_);
	if (this->cap_less_one(desired_depth) == -1) {
		cout << "Error: in Tree::remove(): Tree::cap_less_one(int desired_depth) returns -1\n";
		return;
	}
	int desired_d = this->unq_ - this->cap_less_one(desired_depth) - 1;
	
	//traverse through A
	TreeNode* prev = this->root_;
	int u = 0;
	for (vector<int>::iterator it = A.begin(); it != A.end() - 1; it++) {
		if (*(it) == (int)0) {
			continue;
		}
		int index = (*(it) - 1) % bf;
		if (prev->subtrees_.capacity() == 0) {
			for (int x = 0; x < bf; x++) {
				pair<double, int> new_pair (0, -1);
				TreeNode* new_node = new TreeNode(new_pair);
				prev->subtrees_.insert(prev->subtrees_.begin(), new_node);
			}
		}
		int abs_ind_abv = (n_ssts * bf) / (this->unq_ - 1);
		vector<int> abs_indices;
		for (int index = 0; index < bf; index++) {
			abs_indices.push_back(get<1>(prev->subtrees_.at(index)->data_));
		}
		if (!(any_of (abs_indices.begin(), abs_indices.end(), is_neg_one))) {
			prev = prev->subtrees_.at(index);
		}
		if (u == abs_ind_abv) {
			break;
		} 
		u = *(it++);
	}
	//delete
	int insertion_index = ((this->unq_ - 1) - 1) % bf;
	pair<double, int> new_pair (0, -1);

	prev->subtrees_.at(insertion_index) = new TreeNode(new_pair);
	vector<int> abs_indices;
	for (int index = 0; index < bf; index++) {
		abs_indices.push_back(get<1>(prev->subtrees_.at(index)->data_));
	}
	//reorder
	this->sort();
	return;
}

//Convert the data of a tree into an array-like form.
vector<double> Tree::convert() {
	vector<double> v;
	for (int c_abs_ind = 0; c_abs_ind < this->unq_; c_abs_ind++) {
		pair<double, int> c_pair = this->DFS(c_abs_ind);
		v.push_back(get<0>(c_pair));
	}
	return v;	
}

void Tree::insert(double data, int desired_depth, int desired_d) {
	//These are basic preliminaries not specific to any particular step
	int bf = this->branching_factor_;
	if (this->cap_less_one(desired_depth) == -1) {
		cout << "Error: in Tree::insert(double, int, int): Tree::cap_less_one(desired_depth) returns -1\n";
		return;
	}
	const int d_abs_ind = this->cap_less_one(desired_depth) + desired_d;

	//DFS algorithm here.
	//calculate ch
	int ch = this->height(d_abs_ind);
	int dh;
	dh = desired_depth;
	//calculate i
	vector<int> D; 
	for (int n = cap_less_one(dh); n < cap(dh); n++) {
		D.push_back(n);
	}
	vector<int> C;
	for (int n = cap_less_one(dh); n < d_abs_ind; n++) {
		C.push_back(n);
	}
	int x = 0;
	int n_ssts = 0;
	for (auto& n : C) {
		vector<int>::iterator it = find (D.begin(), D.end(), n);
		if (it != D.end()) {
			x++;
		}
		if (x == bf) {
			x = 0;
			n_ssts++;
		}
	}
	n_ssts += 1;
	int i = n_ssts;
	//catch errors.
	if (this->cap_less_one(dh) == -1) {
		cout << "Error: in Tree::insert(double, int, int): Tree::cap_less_one(dh) returns -1\n";
		return;
	}
	const int l_bnd = this->cap_less_one(dh);
	const int u_bnd = this->cap(dh);
	for (auto& n : C) {
		if (n < l_bnd || n >= u_bnd) {
			cout << "Error: Attempt to create a disconnected graph.\n\n";
			return;
		}
	}
	//create array = A, "path"
	vector<int> A; A.reserve(dh+1);
	A.assign(dh+1, 0);
	A.at(dh) = d_abs_ind;

	int abv = -1;
	for (int index = dh; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = this->unq_ - 1;
			if (ch == 1) {
				continue;
			} else {
				abv = i + (cap_less_one(ch-1) - 1);
			}
		} else {
			A.at(index) = abv;
			if (ch == 1) {
				continue;
			} else if (ch == 2) {
				abv = ( (abv - cap_less_one(ch)) / bf) + 1;
			} else {
				abv = ( (abv - cap_less_one(ch)) / bf ) + cap_less_one(ch-1) + 1;			
			} 
		}
		ch--;
	}
	//traverse through A
	TreeNode* prev = this->root_;
	for (vector<int>::iterator it = A.begin() + 1; it != A.end() - 1; it++) {
		int traversal_index = ( (*it) - 1 ) % bf;
		prev = prev->subtrees_.at(traversal_index);
	}
	//insert
	int insertion_index = (d_abs_ind - 1) % bf;
	pair<double, int> new_pair (data, d_abs_ind);
	if (prev->subtrees_.capacity() == 0) {
		for (int index = 0; index < bf; index++) {
			pair<double, int> new_pair (0, -1);
			TreeNode* new_node = new TreeNode(new_pair);
			prev->subtrees_.insert(prev->subtrees_.begin(), new_node);
		}
	} else if (get<1>(prev->data_) == -1) {
		cout << "Error: in Tree::insert(double, int, int): Attempt to create a disconnected graph.\n\n";
		return;
	}
	prev->subtrees_.at(insertion_index) = new TreeNode(new_pair);
	//reorder
	this->sort();
	return;
}

void Tree::remove(int desired_depth, int desired_d) {
	//These are basic preliminaries not specific to any particular step
	int bf = this->branching_factor_;
	if (this->cap_less_one(desired_depth) == -1) {
		cout << "Error: in Tree::remove(int, int): Tree::cap_less_one(desired_depth) returns -1\n";
		return;
	}
	const int d_abs_ind = this->cap_less_one(desired_depth) + desired_d;

	//DFS algorithm here.
	//calculate ch
	int ch = desired_depth;
	const int dh = desired_depth;
	//calculate i
	vector<int> D; 
	for (int n = cap_less_one(dh); n < cap(dh); n++) {
		D.push_back(n);
	}
	vector<int> C;
	for (int n = cap_less_one(dh); n < d_abs_ind; n++) {
		C.push_back(n);
	}
	int x = 0;
	int n_ssts = 0;
	for (auto& n : C) {
		vector<int>::iterator it = find (D.begin(), D.end(), n);
		if (it != D.end()) {
			x++;
		}
		if (x == bf) {
			x = 0;
			n_ssts++;
		}
	}
	n_ssts += 1;
	int i = n_ssts;

	//create array = A, "path"
	vector<int> A; A.reserve(dh+1);
	A.assign(dh+1, 0);
	A.at(dh) = d_abs_ind;
	
	int abv = -1;
	for (int index = dh; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = this->unq_ - 1;
			if (ch == 1) {
				continue;
			} else {
				abv = i + (cap_less_one(ch-1) - 1);
			}
		} else {
			A.at(index) = abv;
			if (ch == 1) {
				continue;
			} else if (ch == 2) {
				abv = ( (abv - cap_less_one(ch)) / bf) + 1;
			} else {
				abv = ( (abv - cap_less_one(ch)) / bf ) + cap_less_one(ch-1) + 1;			
			} 
		}
		ch--;
	}

	//traverse through A
	TreeNode* prev = this->root_;
	int uh = A.size() - 1;
	int abs_ind_abv = -1;

	//We're going to have two iterators in here - one for traversing forwards and one for 
	//traversing in reverse.
	vector<int>::iterator it1 = A.begin();
	for (vector<int>::iterator it0 = A.end() - 1; it0 != A.begin(); it0--) {
		//keep going
		if (prev->subtrees_.capacity() == 0) {
			for (int index = 0; index < bf; index++) {
				pair<double, int> new_pair (0, -1);
				TreeNode* new_node = new TreeNode(new_pair);
				prev->subtrees_.insert(prev->subtrees_.begin(), new_node);
			}
		}

		if (abs_ind_abv == -1) {
			if (uh - 1 <= 0) {
				abs_ind_abv = n_ssts + 1 + 0 - 1;
			} else {
				abs_ind_abv = n_ssts + 1 + cap_less_one(uh-1) - 1;
			}
		} else {
			if (uh - 1 <= 0) {
				abs_ind_abv = ( *it0 / bf ) + 1 + 0 - 1;
			} else {
				abs_ind_abv = ( *it0 / bf ) + 1 + cap_less_one(uh-1) - 1;
			}
		}

		//
		vector<int> abs_indices;
		for (int index = 0; index < bf; index++) {
			if (prev->subtrees_.capacity() == 0) {
				for (int times = 0; times < bf; times++) {
					abs_indices.push_back(-2);
				}
				break;
			}
			abs_indices.push_back(get<1>(prev->subtrees_.at(index)->data_));
		}

		//find index from which to insert to

		//hypothetical new way to calculate 'index' accurately in all cases.
		int h = this->height(d_abs_ind);
		int n = (d_abs_ind - cap_less_one(h)) / bf;
		while ( !(cap_less_one(h) < *it1 && *it1 <= cap(h)) ) {
			h--;
			if (h-1 <= 0) {
				break;
			}
		}
		if (n != 0) {
			while ( !(cap_less_one(h) <= n && n < cap(h)) ) {
				n /= bf;
			}
		}
		int index = n;

		if (index < 0) {
			index = (*it1) % bf;
		}

		if ( (abs_indices.at(index) != -1 && abs_indices.at(index) != -2) && this->height(*it1)+1 < desired_depth) {
			prev = prev->subtrees_.at(index);
		}
		if ( (abs_indices.at(index) == -1 || abs_indices.at(index) == -2) && this->height(*it1)+1 < desired_depth) {
			cout << "Error: Attempt to create a disconnected graph.\n\n";
			return;
		}

		if (none_of (abs_indices.begin(), abs_indices.end(), g_thn_neg_one)) {
			cout << "Error: Attempt to create a disconnected graph.\n\n";
			return;
		}

		if ( get<1>(prev->data_) == abs_ind_abv) {
			break;
		}
		it1++;
		uh--;
	}
	//insert
	int insertion_index = (d_abs_ind - 1) % bf;
	pair<double, int> _void (0, -1);

	prev->subtrees_.at(insertion_index) = new TreeNode(_void);
	vector<int> abs_indices;
	for (int index = 0; index < bf; index++) {
		abs_indices.push_back(get<1>(prev->subtrees_.at(index)->data_));
	}
	if (all_of (abs_indices.begin(), abs_indices.end(), is_neg_one)) {
		prev->subtrees_ = vector<TreeNode*>();
	}
	//reorder
	this->sort();
	return;
}

//insert a void (alloc)
//Seek the first null space. Insert a void to this space.
void Tree::insert(void* blank) {
	//Determine the location of the first null space
	int node = 0;
	TreeNode* r = this->root_;
	while (get<1>(r->data_) != -2) {
		node++;
		r = DFST(node);
	}
	//DFS algorithm here.
	//calculate ch, dh
	int ch = this->height(node);
	int bf = this->branching_factor_;
	const int dh = ch;
	if (this->cap_less_one(dh) == -1) {
		cout << "Error: in Tree::insert(void*): Tree::cap_less_one(dh) returns -1\n";
		return;
	}
	const int desired_d = node - this->cap_less_one(dh);
	const int d_abs_ind = this->cap_less_one(dh) + desired_d;
	//calculate i
	vector<int> D; 
	for (int n = cap_less_one(dh); n < cap(dh); n++) {
		D.push_back(n);
	}
	vector<int> C;
	for (int n = cap_less_one(dh); n < d_abs_ind; n++) {
		C.push_back(n);
	}
	int x = 0;
	int n_ssts = 0;
	for (auto& n : C) {
		vector<int>::iterator it = find (D.begin(), D.end(), n);
		if (it != D.end()) {
			x++;
		}
		if (x == bf) {
			x = 0;
			n_ssts++;
		}
	}
	n_ssts += 1;
	int i = n_ssts;
	//create array = A, "path"
	vector<int> A; A.reserve(dh+1);
	A.assign(dh+1, 0);
	A.at(dh) = d_abs_ind;
	
	int abv = -1;
	for (int index = dh; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = d_abs_ind;
			if (ch == 1) {
				continue;
			} else {
				abv = i + (cap_less_one(ch-1) - 1);
			}
		} else {
			A.at(index) = abv;
			if (ch == 1) {
				continue;
			} else if (ch == 2) {
				abv = ( (abv - cap_less_one(ch)) / bf) + 1;
			} else {
				abv = ( (abv - cap_less_one(ch)) / bf ) + cap_less_one(ch-1) + 1;			
			} 
		}
		ch--;
	}

	//traverse through A
	TreeNode* prev = this->root_;
	for (vector<int>::iterator it = A.begin() + 1; it != A.end() - 1; it++) {
		int traversal_index = ( (*it) - 1 ) % bf;
		prev = prev->subtrees_.at(traversal_index);
	}
	//insert
	int insertion_index = (d_abs_ind - 1) % bf;
	pair<double, int> _void (0, -1);
	if (prev->subtrees_.capacity() <= bf) {
		TreeNode* b = new TreeNode(_void);
		prev->subtrees_.insert(prev->subtrees_.begin(), b);
	}
	return;
}

//Determine if a tree is balanced
//(Determine if full node system is balanced)
bool Tree::is_balanced() { 
	//Search the entire tree to gather data of its height
	//Return this->size_ < pow(branching_factor, height) 
	int height = this->height();
	int capacity = 0;
	while (height >= 0) {
		capacity += pow (this->branching_factor_, height);
		height--;
	}
	return this->size_ == capacity;
}

//Determine if a tree, up until a certain height, has exactly zero 
//null or void nodes present. Return true if so; return false if
//there exists at least one null or void node within capacity.
bool Tree::is_balanced(int h) {
	const int cap = this->cap(h);
	for (int node = 0; node < cap; node++) {
		pair<double, int> p = DFS(node);
		if (get<1>(p) == -1 || get<1>(p) == -2) {
			return false;
		}
	}
	return true;
}

//Determines if the memory allocated to this tree is balanced
bool Tree::is_alloc_bal() {
	int height = this->height();
	int capacity = 0;
	while (height >= 0) {
		capacity += pow (this->branching_factor_, height);
		height--;
	}
	return this->unq_ == capacity;
}

//Balance the tree by allocating the proper number of nodes
void Tree::alloc_by_bal() {
	const int dh = this->height(this->unq_);
	const int dcap = this->cap(dh);
	const int fill = dcap - this->alc_unq_;
	for (int node = 0; node < fill; node++) {
		this->insert((void*)0);
	}	
	this->alc_unq_ = this->count_alc();
}

//Add a new layer of void nodes to a tree
void Tree::alloc_lvl() {
	if (this->root_ == NULL) {
		insert((void*)0);
		return;
	} else if (!is_alloc_bal()) {
		cout << "Error: in Tree::alloc_lvl(): cannot allocate a level to an unbalanced tree\n";
		return;
	} else {
		//get the height of the tree
		//insert in the amount of pow (this->branching_factor, height+1)
		const int h = this->alc_ht();
		for (int index = 0; index < pow (this->branching_factor_, h + 1); index++) {
			this->insert((void*)0);
		}
	}
	//address changes
	this->alc_unq_ = this->count_alc();
}

//return the height of the tree.
//This function returns the height of unq_. 
int Tree::height() {
	int hc = 1;
	if ( (this->unq_ - 1) == 0) {
		return 0;
	}

	while (!(this->cap_less_one(hc) <= (this->unq_ - 1) && (this->unq_ - 1) < this->cap(hc))) {
		hc++;
	}	

	return hc;

}

//return the height of the absolute index.
int Tree::height(int d_abs_ind) {
	int hc = 1;
	if (d_abs_ind == 0) {
		return 0;
	}
	if (d_abs_ind == 1) {
		return 1;
	}
	while (!(this->cap_less_one(hc) <= d_abs_ind && d_abs_ind < this->cap(hc))) {
		hc++;
	}	
	return hc;

}

//Determine the height of the first unallocated level
int Tree::unalc_ht() {	
	int ch = 0;
	int ccap = 1;
	for (int index = 0 ; index < this->unq_; index++) {
		if (ccap == this->cap(ch)) {
			ch++;
			ccap = this->cap(ch+1);
		}
		pair<double, int> r = DFS(index);
		if (get<1>(r) == -1) {
			break;			
		}
	}	
	return ch;
}

//return the height of the highest allocated node
int Tree::alc_ht() {
	int ch = 0;
	int ccap = 1;
	int index = 0;
	pair<double, int> r (0, -1);
	while (get<1>(r) != 0) {
		if (ccap == this->cap(ch)) {
			ch++;
			ccap = this->cap(ch+1);
		}
		r = DFS(index);
		index++;
	}	
	return ch+1;
}

//Search the tree for the absolute index
//Return the pair associated with it
//Return a null pair (i.e. with data of {(double)0, (int)-2}) if the index is not found.
pair<double, int> Tree::DFS(int d_abs_ind) {
	//root case
	if (d_abs_ind == 0) {
		return this->root_->data_;
	}
	//get height of abs_index
	const int bf = this->branching_factor_;
	int c_height = this->height(d_abs_ind);
	const int d_height = c_height;
	//get capacity
	if (cap_less_one(d_height) == -1) {
		cout << "Error: in Tree::DFS: Tree::cap_less_one(d_height) returns -1\n";
		pair<double, int> null_p (0, -2);
		TreeNode* null_t = new TreeNode(null_p);
		return null_t->data_;
	}
	int capacity = this->cap_less_one(d_height);
	//determine i
	vector<int> B;
	for (int n = cap_less_one(d_height); n < cap(d_height); n++) {
		B.push_back(n);
	}
	vector<int> C;
	for (int n = cap_less_one(d_height); n < d_abs_ind; n++) {
		C.push_back(n);
	}
	int x = 0;
	int n_ssts = 0;
	for (auto& n : C) {
		vector<int>::iterator it = find (B.begin(), B.end(), n);
		if (it != B.end()) {
			x++;
		}
		if (x == bf) {
			x = 0;
			n_ssts++;
		}
	}
	n_ssts += 1;
	int i = n_ssts;
	//use formula
	vector<int> A; A.reserve(d_height+1);
	A.assign(d_height+1, 0);
	A.at(d_height) = d_abs_ind;

	if (this->cap_less_one(c_height) == -1) {
		cout << "Error: in DFS(): Tree::cap_less_one(c_height) returns -1\n";
		pair<double, int> null_p (0, -2);
		TreeNode* null_t = new TreeNode(null_p);
		return null_t->data_;
	}
	//create A
	int abv = -1;
	for (int index = d_height; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = d_abs_ind;
			if (c_height == 1) {
				continue;
			} else {
				abv = i + (cap_less_one(c_height-1) - 1);
			}
		} else {
			A.at(index) = abv;
			if (c_height == 1) {
				continue;
			} else if (c_height == 2) {
				abv = ( (abv - cap_less_one(c_height)) / bf) + 1;
			} else {
				abv = ( (abv - cap_less_one(c_height)) / bf ) + cap_less_one(c_height-1) + 1;			
			} 
		}
		c_height--;
	}

	//traverse A
	TreeNode* prev = this->root_;
	for (vector<int>::iterator it = A.begin() + 1; it != A.end(); it++) {
		int traversal_index = ( (*it) - 1 ) % bf;
		if (get<1>(prev->data_) == d_abs_ind) {
			break;
		} else if (prev->subtrees_.size() <= traversal_index) {
			pair<double, int> n_p (0, -2);
			return n_p;

		} else {
			prev = prev->subtrees_.at(traversal_index);
		}

	}
	return prev->data_;

}

//Search the tree for the absolute index
//Return the TreeNode associated with it
//Return a null treenode (i.e. with data of {(double)0, (int)-2}) if the index is not found.
TreeNode* Tree::DFST(int d_abs_ind) {
	//root case
	if (d_abs_ind == 0) {
		return this->root_;
	}
	//get height of abs_index
	const int bf = this->branching_factor_;
	int c_height = 1;
	while (!(cap_less_one(c_height) <= d_abs_ind && d_abs_ind < cap(c_height))) {
		c_height++;
	}
	const int d_height = c_height;
	//get capacity
	if (cap_less_one(d_height) == -1) {
		cout << "Error: in Tree::DFST: Tree::cap_less_one(d_height) returns -1\n";
		pair<double, int> null_p (0, -2);
		TreeNode* null_t = new TreeNode(null_p);
		return null_t;
	}
	int capacity = this->cap_less_one(d_height);
	//determine i
	vector<int> B;
	for (int n = cap_less_one(d_height); n < cap(d_height); n++) {
		B.push_back(n);
	}
       	vector<int> C;
	for (int n = cap_less_one(d_height); n < d_abs_ind; n++) {
		C.push_back(n);
	}
	int x = 0;
	int n_ssts = 0;
	for (auto& n : C) {
		vector<int>::iterator it = find (B.begin(), B.end(), n);
		if (it != B.end()) {
			x++;
		}
		if (x == bf) {
			x = 0;
			n_ssts++;
		}
	}
	n_ssts += 1;
	int i = n_ssts;
	//use formula
	vector<int> A; A.reserve(d_height+1);
	A.assign(d_height+1, 0);
	A.at(d_height) = d_abs_ind;
	if (this->cap_less_one(c_height) == -1) {
		cout << "Error: in DFST(): Tree::cap_less_one(c_height) returns -1\n";
		pair<double, int> null_p (0, -2);
		TreeNode* null_t = new TreeNode(null_p);
		return null_t;
	}

	int abv = -1;
	for (int index = d_height; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = d_abs_ind;
			if (c_height == 1) {
				continue;
			} else {
				abv = i + (cap_less_one(c_height-1) - 1);
			}
		} else {
			A.at(index) = abv;
			if (c_height == 1) {
				continue;
			} else if (c_height == 2) {
				abv = ( (abv - cap_less_one(c_height)) / bf) + 1;
			} else {
				abv = ( (abv - cap_less_one(c_height)) / bf ) + cap_less_one(c_height-1) + 1;			
			}
		}
		c_height--;
	}

	TreeNode* prev = this->root_;
	for (vector<int>::iterator it = A.begin() + 1; it != A.end(); it++) {
		int traversal_index = ( (*it) - 1 ) % bf;
		if (get<1>(prev->data_) == d_abs_ind) {
			break;
		} else if (prev->subtrees_.size() <= traversal_index) {
			pair<double, int> n_p (0, -2);
			TreeNode* n_t = new TreeNode(n_p);
			return n_t;
		} else {
			prev = prev->subtrees_.at(traversal_index);
		}
	}
	return prev;

}

//Search using BFS
//Return the pair associated if the node is found
//Return the null node (double 0, int -2) if the node is not found
pair<double, int> Tree::BFS(int abs_index) {
	queue<TreeNode*> line;
	queue<TreeNode*> copy;
	bool base_case = true;
	do {
		//base case
		if (base_case == true) {
			for (auto& treenode : this->root_->subtrees_) {
				line.push(treenode);
				if (get<1>(treenode->data_) == abs_index) {
					return treenode->data_;
				}
			}
		}
		//new line
		while (!line.empty()) {
			TreeNode* popped = line.front();
			for (auto& treenode : popped->subtrees_) {
				copy.push(treenode);
				if (get<1>(treenode->data_) == abs_index) {
					return treenode->data_;
				}
			}
			line.pop();
		}
		//reset
		swap(line, copy);
		base_case = false;
	} while (line.empty() != true);
	cout << "Error: in Tree::BFS(int): Argument not found\n\n";
	pair<double, int> n_p (0, -2);
	return n_p;
}


//new sort 2
//Use DFS to sort
void Tree::sort() {
	int unq = 0;
	int alc_unq = 0;
	int size = 0;
	int bf = this->branching_factor_;
	int abs_ind = 1;
	int ef_ch = 1;
	if (this->root_ != NULL) {
		size++;
	} else {
		return;
	}
	while (ef_ch < this->height(this->alc_unq_) + 1) {
		vector<int> A;
		A.assign(ef_ch + 1, 0);
		//Reset or create A
		int i = ( (abs_ind - this->cap_less_one(this->height(abs_ind))) / bf) + 1;
		int abv = -1;
		for (int ch = ef_ch; ch > 0; ch--) {
			if (abv == -1) {
				A.at(ch) = abs_ind;
				if (ch == 1) {
					continue;
				} else {
					if (ch - 1 < 0) {
						if (ef_ch == this->height(abs_ind)) {
							abv = i + (0 - 1);
						} else {
							abv = (abv - cap_less_one(this->height(abv)) / bf) + 1 + (0 - 1);
						}
					} else {
						if (ef_ch == this->height(abs_ind)) {
							abv = i + (cap_less_one(ch-1) - 1);
						} else {
							abv = (abv - cap_less_one(this->height(abv)) / bf) + 1 + (cap_less_one(ch-1) - 1);
						}
					}
				}
			} else {
				A.at(ch) = abv;
				if (ch == 1) {
					continue;
				} else if (ch == 2) {
					abv = ( (abv - cap_less_one(ch)) / bf) + 1;
				} else {
					abv = ( (abv - cap_less_one(ch)) / bf ) + cap_less_one(ch-1) + 1;			
				} 
			}
		}	
		
		//Traverse A
		while (abs_ind < cap(ef_ch)) {
			//Traverse A
			TreeNode* prev = this->root_;
			int exit = -1;
			for (vector<int>::iterator it = A.begin() + 1; it != A.end() - 1; it++) {
				int traversal_index = (*(it) - 1) % bf;
				if (traversal_index < prev->subtrees_.size()) {
					prev = prev->subtrees_.at(traversal_index);
				} else {
					break;
				}
			}
			//do something
			int ins = (abs_ind - 1) % bf;
			if (ins < prev->subtrees_.size()) {
				if (get<1>(prev->subtrees_.at(ins)->data_) > -1) {
					get<1>(prev->subtrees_.at(ins)->data_) = abs_ind;				

					size++;
					unq = abs_ind;
					alc_unq = abs_ind;	
				} else if (get<1>(prev->subtrees_.at(ins)->data_) == -1) {
					alc_unq = abs_ind;
				}
			}
			//Transform A
			abs_ind++;
			A.at(ef_ch) = abs_ind;
			r(ef_ch, bf, A);
		}
		ef_ch++;
	}
	this->set_unq(unq + 1);	
	this->set_size(size);
	this->set_alc_unq(alc_unq + 1);
}

void Tree::r(int n, int bf, vector<int>& A) {
	if ((A.at(n) - 1) % bf == 0 && n > 1) {
		if (this->cap_less_one(n) < A.at(n)) {
			A.at(n-1) += 1;
		}
		r(n-1, bf, A);
	}
}


//Use DFS to count the alc_unq_, or the unq_ should it measure even void nodes.
//This does not return a zero-indexed value.
int Tree::count_alc() {
	int node = 0;
	int ch = 1;
	pair<double, int> unalc_p (0, -2);
	TreeNode* unalc = new TreeNode(unalc_p);
	vector<TreeNode*> line;
	vector<TreeNode*> prev_line;
	vector<int> abs_indices;
	vector<int> prev_abs_indices;

	//base case and inductive case.
	if (this->root_->subtrees_.capacity() == 0) {
		return 1;
	}
	//Search by height until the line in question has not a single allocated node.
	do {
		abs_indices.clear();
		line.clear();	

		int size = pow (this->branching_factor_, ch);
		abs_indices.reserve(size);
		line.reserve(size);

		for (int treenode = cap_less_one(ch); treenode < cap(ch); treenode++) {	
			TreeNode* r = DFST(treenode);
			if (r == NULL) {
				line.push_back(unalc);
			} else {
				line.push_back(r);
			}
		}

		for (int abs_index = 0; abs_index < cap(ch) - cap_less_one(ch); abs_index++) {
			abs_indices.push_back(get<1>(line.at(abs_index)->data_));		
		}

		if ( none_of (abs_indices.begin(), abs_indices.end(), g_thn_neg_one) ) {
			break;
		} else {
			prev_line = line;
			prev_abs_indices = abs_indices;
			ch++;
		}
	} while	( !( none_of (abs_indices.begin(), abs_indices.end(), g_thn_neg_one) ) );
	//We shall check to see what the unq_ of prev is
	int d;
	int n = 0;
	int prev_size = pow (this->branching_factor_, ch-1);
	while (n < prev_size) {
		if (prev_abs_indices.at(n) > -2) {
			d = n;
		}
		n++;
	}

	int new_alc_unq = this->cap_less_one(ch-1) + d + 1;
	return new_alc_unq;
}

void Tree::set_branching_factor(int data) {
	if (this->root_ == NULL) {
		this->branching_factor_ = data;
	} else {
		cout << "Error: Cannot change branching factor of non-empty tree\n";
	      	return;
	}
}

int Tree::get_branching_factor() {
	return this->branching_factor_;
}

void Tree::set_size(int data) {
	this->size_ = data;
}

void Tree::increment_size() {
	this->size_++;
}

void Tree::decrement_size() {
	this->size_--;
}

void Tree::increment_alc_unq() {
	this->alc_unq_++;
}

void Tree::decrement_alc_unq() {
	this->alc_unq_--;
}

int Tree::get_size() {
	return this->size_;
}

void Tree::increment_unq() {
	this->unq_++;
}

void Tree::set_unq(int data) {
	this->unq_ = data;
}

int Tree::get_unq() {
	return this->unq_;
}

int Tree::get_alc_unq() {
	return this->alc_unq_;
}

void Tree::set_alc_unq(int data) {
	this->alc_unq_ = data;
}

bool is_neg_one(int i) {
	return i == -1;
}

bool g_thn_zro(int i) {
	return i >= 0;
}

bool g_thn_neg_one(int i) {
	return i >= -1;
}

/*
bool line_all_null(queue<TreeNode*> l) {
	TreeNode* treenode;
	while (l.empty() != true) {
		treenode = l.front();
		if ( get<1>(treenode->data_) >= 0 ) {
			return false;
		}	
		l.pop();
	}
	return true;
}
*/

int Tree::cap_less_one(int h) {
	//added
	if (h == 1) {
		return 1;
	} else if (h < 1) {
		cout << "Error: Tree::cap_less_one(int): Invalid argument\n";
		return -1;
	}
	
	int capacity = 0;
	for (int height = 0; height < h; height++) {
		capacity += pow (this->branching_factor_, height);
	}
	return capacity;
}

int Tree::cap(int h) {
	//added
	if (h == 0) {
		return 0;
	} else if (h < 0) {
		cout << "Error: Tree::cap(int): Invalid argument\n";
		return -1;
	}	
	
	int capacity = 0;
	for (int height = 0; height <= h; height++) {
		capacity += pow (this->branching_factor_, height);
	}
	return capacity;
}

void tree_test_code() {
	//Test insert
	Tree tree0 = Tree();
	for (int num = 5; num < 13; num++) {
		tree0.insert(num);
	}
	cout << "This should read \'5 6 7 8 9 10 11 12\':\n";
	tree0.print();
	
	//Test remove
	tree0.remove();
	tree0.remove();
	tree0.remove();
	cout << "This should read \'5 6 7 8 9\':\n\n";
	tree0.print();
	
	//test convert()
	vector<double> array; array.reserve(tree0.get_size()); 
	array = tree0.convert();
	int index = 0;
	cout << "This should read \'5 6 7 8 9\':\n\n";
	for (auto& index : array) {
		cout << index << " ";
	} cout << "\n";
	Tree tree1 = Tree();
		
	//Test Tree::operator=
	tree1 = tree0; 
	cout << "This should read \'5 6 7 8 9\':\n\n";
	tree1.print(); 
	
	//Test convert() 
	vector<double> array1; array1 = tree1.convert();
	cout << "This should read \'5 6 7 8 9\':\n\n";
	for (auto& index : array1) {
		cout << index << " ";
	} cout << "\n";
	
	//Test insert_here
	tree1.insert((double)15, 2, 2);
	cout << "This (the unq_ of the tree, which is " << tree1.get_unq() << ") should be \'7\'.\n";
	cout << "This (the alc_unq_ of the tree, which is " << tree1.get_alc_unq() << ") should be \'7\'.\n";

	cout << "Also, this should read \'5 6 7 8 9 15\':\n\n";
	tree1.print(); 
	tree1.insert((double)20, 2, 7);
	cout << "This (the unq_ of the tree, which is " << tree1.get_unq() << ") should be \'12\'.\n";
	cout << "This (the alc_unq_ of the tree, which is " << tree1.get_alc_unq() << ") should be \'13\'.\n";

	cout << "This should read \'5 6 7 8 9 15 20\':\n\n";
	tree1.print();
	Tree tree2 = Tree();
	tree2.insert(1);
	tree2.insert((double)1, 1, 2);
	cout << "This should read \'1 1\':\n\n";
	tree2.print(); 
	
	//test remove_here
	tree2.remove(1, 2);
	cout << "This should read \'1\':\n\n";
	tree2.print();
	tree2.insert((double)1, 1, 2);
	
	//Test alloc_lvl
	cout << "What is this (the unq_ of the tree)? It\'s " << tree2.get_unq() << "\n";
	cout << "This (the alc_unq_ of the tree) should read \'4\': " << tree2.get_alc_unq() << "\n";
	tree2.alloc_lvl();
	cout << "What is this (the unq_ of the tree)? It\'s " << tree2.get_unq() << "\n";
	cout << "This (the alc_unq_ of the tree) should read \'13\' now: " << tree2.get_alc_unq() << "\n";
	tree2.alloc_by_bal();
	tree2.alloc_lvl();
	cout << "This should read \'1 1\':\n\n";
	tree2.print(); 
	
	//Test insert_here error message for creating a disconnected graph
	cout << "This should read \'Error: attempt to create a disconnected graph\':\n\n";
	tree2.insert(1, 2, 3);
	tree2.print();
	
	//Test branching factor
	Tree tree3 = Tree();
	tree3.set_branching_factor(4);
	for (int index = 44; index < 59; index++) {
		tree3.insert(index);
	}
	cout << "This should read \'44 45\', 44-58.\n";
	tree3.print();
	
	//Test assignment
	Tree tree4 = Tree(tree3);
	cout << "This should also read \'44 45\', 44-58.\n\n";
	tree4.print();
	
	//test getters and setters
	cout << "Getting branching factor. This should read \'4\': " << tree4.get_branching_factor() << "\n";
	//test remove_here
	cout << "Should read \'44 45 46 47 48 49 50 51 52 53 55 56 57 58\':\n\n";
	tree4.remove(2, 5);
	tree4.print();
	cout << "Now it should read \'44 45 46 48 49 50 51 52 53 55 56\':\n\n";
	tree4.remove(1, 2);
	tree4.print();
	cout << "Now it should read \'44 46 48 53 55 56\':\n\n";
	tree4.remove(1, 0);
	tree4.print();
	//test insert_here
	cout << "Now it should read \'44 1.23 46 48 53 55 56\':\n\n";
	tree4.insert(1.23, 1, 0);
	tree4.print();
	cout << "Now it should read \'44 1.23 46 48 4.56 53 55 56\':\n\n";
	tree4.insert(4.56, 2, 0);
	tree4.print();

	
	//test remove
	cout << "Now it should read \'44 1.23 46 48 4.56 55 56\':\n\n";
	tree4.remove(2, 4);
	tree4.print();
	
	//test insert
	cout << "Now it should read \'44 1.23 46 7.89 48 4.56 55 56\':\n\n";
	tree4.insert(7.89);
	tree4.print();

	//test balance
	cout << "The alc_unq_ currently is \'" << tree4.get_alc_unq() << "\'; it should be \'" << 13 << "\'\n\n";
	tree4.alloc_by_bal();
	cout << "The alc_unq_ now is \'" << tree4.get_alc_unq() << "\'; it should be \'" << 21 << "\'\n\n";
	tree4.print();
	//test DFS
	Tree tree5 = Tree();
	for (int num = 0; num < 40; num++) {
		tree5.insert(num);
	}
		
	pair<double, int> main_pair;
	main_pair = tree5.DFS(14);
	cout << "This should be (14, 14):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	main_pair = tree5.DFS(38);
	cout << "This should be (38, 38):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	main_pair = tree5.DFS(26);
	cout << "This should be (26, 26):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	main_pair = tree5.DFS(2);
	cout << "This should be (2, 2):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	main_pair = tree5.DFS(0);
	cout << "This should be (0, 0):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";

	//test DFS
		
	Tree tree6 = Tree();
	for (int num = 0; num < 100; num++) {
		tree6.insert(num);
	}
	pair<double, int> alt_pair;
	alt_pair = tree6.DFS(75);
	cout << "This should be (75, 75):\n\n";
	cout << "("<<get<0>(alt_pair)<<", "<<get<1>(alt_pair)<<")\n";
	alt_pair = tree6.DFS(99);
	cout << "This should be (99, 99):\n\n";
	cout << "("<<get<0>(alt_pair)<<", "<<get<1>(alt_pair)<<")\n";

	//test DFS
	Tree tree7 = Tree();
	for (int num = 0; num < 150; num++) {
		tree7.insert(num);
	}
	pair<double, int> pair2;
	pair2 = tree7.DFS(125);
	cout << "This should be (125, 125):\n\n";
	cout << "("<<get<0>(pair2)<<", "<<get<1>(pair2)<<")\n";
	pair2 = tree7.DFS(149);
	cout << "This should be (149, 149):\n\n";
	cout << "("<<get<0>(pair2)<<", "<<get<1>(pair2)<<")\n";
	//test DFS
	Tree tree8 = Tree();
	for (int num = 0; num < 300; num++) {
		tree8.insert(num);
	}
	pair<double, int> pair3;
	pair3 = tree8.DFS(225);
	cout << "This should be (225, 225):\n\n";
	cout << "("<<get<0>(pair3)<<", "<<get<1>(pair3)<<")\n";
	pair3 = tree8.DFS(299);
	cout << "This should be (299, 299):\n\n";
	cout << "("<<get<0>(pair3)<<", "<<get<1>(pair3)<<")\n";
	cout << "This should be (999, 999): \n\n";
	Tree tree9 = Tree();
	for (int num = 0; num < 1000; num++) {
		tree9.insert(num);
	}
	pair3 = tree9.DFS(998);
	cout << "This should be (998, 998):\n\n";
	cout << "("<<get<0>(pair3)<<", "<<get<1>(pair3)<<")\n";
}
