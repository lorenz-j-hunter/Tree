//Tree.cpp. Reminder - this tree is simple, connected, and directed. It has set properties.
//By Lorenz Hunter
//Spring 2025

#include "Tree.h"
#include <queue>
#include <iostream>
#include "math.h"
#include <chrono>

using std::queue;
using std::cout;
using namespace std::chrono;

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

//Insert a data into the tree to the first void space.
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
	const int orig_unq = this->unq_;
	int bf = this->branching_factor_;
	//calculate dh
	int h = 0; 
	int a = 0;
	//1. begin at the first void index or 0. End at the next void index or this->unq_.
	for (int abs_ind = this->fv_; abs_ind < this->unq_; abs_ind++) {
		pair<double, int> data = NDFS(abs_ind);
		if (get<1>(data) == -1) {
			h = this->height(a);
			this->fv_ = abs_ind;
			break;
		}
		if (abs_ind == this->unq_-1) {
			a++;
			this->fv_ = 0;
			h = this->height();
			break;
		}

		a++;
	}
	//2. calculate dh
	int dh;
	if (this->is_balanced(h)) {
		dh = h + 1;
		h = h + 1;
	} else if (!this->is_balanced(h)) {
		dh = h;
	}	
	const int desired_depth = dh;	

	int cap_less_one_ = this->cap_less_one(dh);
	const int d_abs_ind = a;

	//create and traverse path 
  int future = -1;
  int offset = 0;
  int clo_ = 0;
  int blw = -1;
  int cap_ = 0;
  int cur = 0;
	TreeNode* cur_node = this->root_;
  for (int ch = 1; ch < dh; ch++) { //stop just before index
    if (blw == -1) {
      clo_ = 1; 
      cap_ = 1 + bf;

      // base case: future == -1

			double bf_d = static_cast<double>(bf);
			double denom = 1.0 / bf_d;

      /*Create valid traversal indices.*/
      // create cur
      cur = std::floor(((d_abs_ind - cap_less_one_) / std::pow(bf, dh)) / denom) + 1;

			//determine near_begin for ch+1.
      double r_cur = (cur - clo_ + 1) / std::pow(bf, ch);
      double near_begin_d = std::floor(r_cur / (1 / std::pow(bf, ch))) * bf; // begin of range
			int near_begin = static_cast<int>(near_begin_d);
      int near_end = near_begin + bf; //end of range

      // traverse to cur here.
			int trav_index = (cur - 1) % bf;
			cur_node = cur_node->subtrees_.at(trav_index);

      blw = 0;
			
      if (ch == dh) {
				break;
			}
      // create offst, future
      int d = d_abs_ind - cap_less_one_;
      int frame = pow(bf, dh-1);
      offset = std::floor(d / frame) * frame;
      future = offset;

      clo_ = 0;
      cap_ = 1;

		} else {
      // ch is currently that for blw, not cur.
      // recursive: future > 0
      cap_ += pow(bf, ch-1); // same lvl as cur, currently.
      clo_ += pow(bf, ch-2); // same lvl as cur, currently.

      /*Find valid traversal index range.*/
      // create valid traversal range.
      double r_cur = (cur - clo_) / std::pow(bf, ch); // near
      double near_begin_d = std::floor(r_cur / (1 / std::pow(bf, ch))) * bf; // begin of range
      int near_begin = static_cast<int>(near_begin_d); 
      int near_end = near_begin + bf; // end of valid range

      /*Hone in.*/
      // find frame
      int frame = std::pow(bf, (dh-ch+1)); // far
      int end = offset + frame; // far

      // hone in with a ratio.
			double num = d_abs_ind - cap_less_one_ - offset;
      double r = num / (end - offset); // far
      int addition = std::floor(r * bf); // near

      /*Select a valid index*/
      // valid index is 'addition'.
      if (ch == dh) {
        blw = cap_ + offset + addition; // near
			} else {
        blw = cap_ + near_begin + addition; // near
			}

			//traverse to blw.
			int trav_index = (blw - 1) % bf;

			cur_node = cur_node->subtrees_.at(trav_index);

      // future 
      cur = blw;
      future = addition * pow(bf, (dh-ch)); // far
      offset += future; // far
		}
	}

	//insert
	int insertion_index;
	if (d_abs_ind == this->unq_ - 1) {
		insertion_index = (d_abs_ind + 1 - 1) % bf;
	} else {
		insertion_index = (d_abs_ind - 1) % bf;
	}
	pair<double, int> new_pair (data, orig_unq);

	if (cur_node->subtrees_.capacity() == 0) {
		for (int index = 0; index < bf; index++) {
			pair<double, int> new_pair (0, -1);
			TreeNode* new_node = new TreeNode(new_pair);
			cur_node->subtrees_.insert(cur_node->subtrees_.begin(), new_node);
		}
	}
	cur_node->subtrees_.at(insertion_index) = new TreeNode(new_pair);
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
		pair<double, int> current = NDFS(node);
		if (get<0>(current) != 0) {
			cout << get<0>(current) << " ";
		}
	}
}

//Remove the element at this->unq_ - 1
void Tree::remove() {
	//root case
	if (this->root_ == NULL) {
		return;
	}
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

	int d_abs_ind = pow(bf, desired_depth) + desired_d;
	if (d_abs_ind < this->fv_) {
		this->fv_ = d_abs_ind;
	}

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
		pair<double, int> c_pair = this->NDFS(c_abs_ind);
		v.push_back(get<0>(c_pair));
	}
	return v;	
}

void Tree::insert(double data, int desired_depth, int desired_d) {
	//These are basic preliminaries not specific to any particular step
	int bf = this->branching_factor_;
	int cap_less_one_ = this->cap_less_one(desired_depth);
	if (cap_less_one_ == -1) {
		cout << "Error: in Tree::insert(double, int, int): Tree::cap_less_one(desired_depth) returns -1\n";
		return;
	}
	const int d_abs_ind = this->cap_less_one(desired_depth) + desired_d;

	//DFS algorithm here.
	//calculate ch
	int ch = this->height(d_abs_ind);
	int dh;
	dh = desired_depth;
	//catch errors.
	if (cap_less_one_ == -1) {
		cout << "Error: in Tree::insert(double, int, int): Tree::cap_less_one(dh) returns -1\n";
		return;
	}
	//create and traverse path 
  int future = -1;
  int offset = 0;
  int clo_ = 0;
  int blw = -1;
  int cap_ = 0;
  int cur = 0;
	TreeNode* cur_node = this->root_;
  for (int ch = 1; ch < dh; ch++) { //stop just before index
    if (blw == -1) {
      clo_ = 1; 
      cap_ = 1 + bf;
      // base case: future == -1

			double bf_d = static_cast<double>(bf);
			double denom = 1.0 / bf_d;

      /*Create valid traversal indices.*/
      // create cur
      cur = std::floor(((d_abs_ind - cap_less_one_) / pow(bf, dh)) / denom) + 1;
      float r_cur = (cur - clo_ + 1) / pow(bf, ch);
      int near_begin = floor(r_cur / (1 / pow(bf, ch))) * bf; // begin of range
      int near_end = near_begin + bf; //end of range

      // traverse to cur here.
			int trav_index = (cur - 1) % bf;
			cur_node = cur_node->subtrees_.at(trav_index);

      blw = 0;

      if (ch == dh) {
				break;
			}
      // create offst, future
      float d = d_abs_ind - cap_less_one_;
      int frame = pow(bf, dh-1);
      offset = floor(d / frame) * frame;
      future = offset;

      clo_ = 0;
      cap_ = 1;

		} else {
      // ch is currently that for blw, not cur.
      // recursive: future > 0
      cap_ += pow(bf, ch-1); // same lvl as cur, currently.
      clo_ += pow(bf, ch-2); // same lvl as cur, currently.

      /*Find valid traversal index range.*/
      // create valid traversal range.
			double cur_num = cur - clo_;
      double r_cur = cur_num / std::pow(bf, ch); // near
      int near_begin = std::floor(r_cur * std::pow(bf, ch)) * bf; // begin of range
      int near_end = near_begin + bf; // end of valid range


      /*Hone in.*/
      // find frame
      int frame = std::pow(bf, (dh-ch+1)); // far
      int end = offset + frame; // far

      // hone in with a ratio.
			double num = d_abs_ind - cap_less_one_ - offset;
      double r = num / (end - offset); // far
      int addition = std::floor(r * bf); // near

      /*Select a valid index*/
      // valid index is 'addition'.
      if (ch == dh) {
        blw = cap_ + offset + addition; // near
			} else {
        blw = cap_ + near_begin + addition; // near
			}

			//traverse to blw.
			int trav_index = (blw - 1) % bf;
			cur_node = cur_node->subtrees_.at(trav_index);

      // future 
      cur = blw;
      future = addition * pow(bf, (dh-ch)); // far
      offset += future; // far
		}
	}


	//insert
	int insertion_index = (d_abs_ind - 1) % bf;
	pair<double, int> new_pair (data, d_abs_ind);
	if (cur_node->subtrees_.capacity() == 0) {
		for (int index = 0; index < bf; index++) {
			pair<double, int> new_pair (0, -1);
			TreeNode* new_node = new TreeNode(new_pair);
			cur_node->subtrees_.insert(cur_node->subtrees_.begin(), new_node);
		}
	} else if (get<1>(cur_node->data_) == -1) {
		cout << "Error: in Tree::insert(double, int, int): Attempt to create a disconnected graph.\n\n";
		return;
	}
	cur_node->subtrees_.at(insertion_index) = new TreeNode(new_pair);
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

	if (d_abs_ind < this->fv_) {
		this->fv_ = d_abs_ind;
	}

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
	int node = this->fv_;
	/*
	int node = 0;
	TreeNode* r = this->root_;
	while (get<1>(r->data_) != -2) {
		node++;
		r = DFST(node);
	}
	*/
	//DFS algorithm here.
	//calculate ch, dh
	int ch = this->height(node);
	int bf = this->branching_factor_;
	const int dh = ch;
	int cap_less_one_ = this->cap_less_one(dh);
	if (cap_less_one_ == -1) {
		cout << "Error: in Tree::insert(void*): Tree::cap_less_one(dh) returns -1\n";
		return;
	}
	const int desired_d = node - cap_less_one_;
	const int d_abs_ind = cap_less_one_ + desired_d;
	//create and traverse path 
  int future = -1;
  int offset = 0;
  int clo_ = 0;
  int blw = -1;
  int cap_ = 0;
  int cur = 0;
	TreeNode* cur_node = this->root_;
  for (int ch = 1; ch < dh; ch++) { //stop just before index
    if (blw == -1) {
      clo_ = 1; 
      cap_ = 1 + bf;
      // base case: future == -1

			double bf_d = static_cast<double>(bf);
			double denom = 1.0 / bf_d;

      /*Create valid traversal indices.*/
      // create cur
      cur = std::floor(((d_abs_ind - cap_less_one_) / std::pow(bf, dh)) / denom) + 1;
      float r_cur = (cur - clo_ + 1) / pow(bf, ch);
      int near_begin = floor(r_cur / (1 / pow(bf, ch))) * bf; // begin of range
      int near_end = near_begin + bf; //end of range

      // traverse to cur here.
			int trav_index = (cur - 1) % bf;
			cur_node = cur_node->subtrees_.at(trav_index);

      blw = 0;

      if (ch == dh) {
				break;
			}
      // create offst, future
      float d = d_abs_ind - cap_less_one_;
      int frame = pow(bf, dh-1);
      offset = floor(d / frame) * frame;
      future = offset;

      clo_ = 0;
      cap_ = 1;

		} else {
      // ch is currently that for blw, not cur.
      // recursive: future > 0
      cap_ += pow(bf, ch-1); // same lvl as cur, currently.
      clo_ += pow(bf, ch-2); // same lvl as cur, currently.

			double bf_d = static_cast<double>(bf);

      /*Find valid traversal index range.*/
      // create valid traversal range.
			double num_cur = cur - clo_;
      double r_cur = num_cur / std::pow(bf, ch); // near
      int near_begin = std::floor(r_cur * std::pow(bf, ch)) * bf; // begin of range
      int near_end = near_begin + bf; // end of valid range

      /*Hone in.*/
      // find frame
      int frame = std::pow(bf_d, (dh-ch+1)); // far
      int end = offset + frame; // far

      // hone in with a ratio.
			double num = d_abs_ind - cap_less_one_ - offset;
      double r = num / (end - offset); // far
      int addition = std::floor(r * bf); // near

      /*Select a valid index*/
      // valid index is 'addition'.
      if (ch == dh) {
        blw = cap_ + offset + addition; // near
			} else {
        blw = cap_ + near_begin + addition; // near
			}

			//traverse to blw.
			int trav_index = (blw - 1) % bf;
			cur_node = cur_node->subtrees_.at(trav_index);

      // future 
      cur = blw;
      future = addition * pow(bf, (dh-ch)); // far
      offset += future; // far
		}
	}

	//insert
	int insertion_index = (d_abs_ind - 1) % bf;
	pair<double, int> _void (0, -1);
	if (cur_node->subtrees_.capacity() <= bf) {
		TreeNode* b = new TreeNode(_void);
		cur_node->subtrees_.insert(cur_node->subtrees_.begin(), b);
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
		pair<double, int> p = NDFS(node);
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
//It's zero-indexed; the root has height=0.
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
		pair<double, int> r = NDFS(index);
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
		r = NDFS(index);
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
	int cap_less_one_ = this->cap_less_one(d_height);
	//determine i
	vector<int> B;
    int cap_ = cap(d_height);
	for (int n = cap_less_one_; n < cap_; n++) {
		B.push_back(n);
	}
	vector<int> C;
	for (int n = cap_less_one_; n < d_abs_ind; n++) {
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
    cap_less_one_ = cap_less_one(c_height);
    cap_ = cap(c_height);
	int abv = -1;
	for (int index = d_height; index > 0; index--) {
		if (abv == -1) {
			A.at(index) = d_abs_ind;
			if (c_height == 1) {
				continue;
			} else {
				abv = i + (cap_less_one_ - 1);
			}
		} else {
			A.at(index) = abv;
			if (c_height == 1) {
				continue;
			} else if (c_height == 2) {
				abv = ( (abv - cap_) / bf) + 1;
			} else {
				abv = ( (abv - cap_) / bf ) + cap_less_one_ + 1;			
			} 
		}
        cap_less_one_ -= pow(bf, c_height-1);
        cap_ -= pow(bf, c_height);
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
//Return the pair associated with it
//Return a null pair (i.e. with data of {(double)0, (int)-2}) if the index is not found.
pair<double, int> Tree::NDFS(int d_abs_ind) {
	//root case
	if (d_abs_ind == 0) {
		return this->root_->data_;
	}
	//get height of abs_index
	const int bf = this->branching_factor_;
	const int dh = this->height(d_abs_ind);
	const int cap_less_one_ = this->cap_less_one(dh);
	//get capacity
	if (cap_less_one_ == -1) {
		cout << "Error: in Tree::DFS: Tree::cap_less_one(d_height) returns -1\n";
		pair<double, int> null_p (0, -2);
		TreeNode* null_t = new TreeNode(null_p);
		return null_t->data_;
	}

	if (cap_less_one_ == -1) {
		cout << "Error: in DFS(): Tree::cap_less_one(c_height) returns -1\n";
		pair<double, int> null_p (0, -2);
		TreeNode* null_t = new TreeNode(null_p);
		return null_t->data_;
	}
	//create and traverse path 
  int future = -1;
  int offset = 0;
  int clo_ = 0;
  int blw = -1;
  int cap_ = 0;
  int cur = 0;
	TreeNode* cur_node = this->root_;
  for (int ch = 1; ch <= dh; ch++) {
    if (blw == -1) {
      clo_ = 1; 
      cap_ = 1 + bf;

			//case 3: dh == 1
			if (ch == dh) {
				break;
			}
      // base case: future == -1

			double bf_d = static_cast<double>(bf);
			double denom = 1.0 / bf_d;

      /*Create valid traversal indices.*/
      // create cur
      cur = std::floor(((d_abs_ind - cap_less_one_) / std::pow(bf, dh)) / denom) + 1;
      double r_cur = (cur - clo_ + 1) / std::pow(bf, ch);
      double near_begin_d = std::floor(r_cur / (1 / std::pow(bf, ch))) * bf; // begin of range
			int near_begin = static_cast<int>(near_begin_d);
      int near_end = near_begin + bf; //end of range

      // traverse to cur here.
			int trav_index = (cur - 1) % bf;
			if (cur_node->subtrees_.size() <= trav_index) {
				pair<double, int> null_p (0, -2);
				return null_p;
			}
			cur_node = cur_node->subtrees_.at(trav_index);
			if (get<1>(cur_node->data_) == d_abs_ind) {
				break;
			}

      blw = 0;

      // create offst, future
      float d = d_abs_ind - cap_less_one_;
      int frame = pow(bf, dh-1);
      offset = floor(d / frame) * frame;
      future = offset;

      clo_ = 0;
      cap_ = 1;

		} else {
      // ch is currently that for blw, not cur.
      // recursive: future > 0
      cap_ += pow(bf, ch-1); // same lvl as cur, currently.
      clo_ += pow(bf, ch-2); // same lvl as cur, currently.

      /*Find valid traversal index range.*/
			double denom = std::pow(bf, ch);
			double bf_d = static_cast<double>(bf);

      // create valid traversal range.
      double r_cur = (cur - clo_) / denom; // near
      double near_begin_d = std::floor(r_cur * denom) * bf; // begin of range
			int near_begin = static_cast<int>(near_begin_d);
      int near_end = near_begin + bf; // end of valid range


      /*Hone in.*/
      // find frame
      double frame_d = std::pow(bf, (dh-ch+1)); // far
			int frame = static_cast<int>(frame_d);
      int end = offset + frame; // far

      // hone in with a ratio.
			double num = d_abs_ind - cap_less_one_ - offset;
      double r = num / (end - offset); // far
      int addition = std::floor(r * bf_d); // near

      /*Select a valid index*/
      // valid index is 'addition'.
      if (ch == dh) {
        blw = cap_ + offset + addition; // near
			} else {
        blw = cap_ + near_begin + addition; // near
			}

			//traverse to blw.
			int trav_index = (blw - 1) % bf;
			if (cur_node->subtrees_.size() <= trav_index) {
				pair<double, int> null_p (0, -2);
				return null_p;
			}
			cur_node = cur_node->subtrees_.at(trav_index);
			if (get<1>(cur_node->data_) == d_abs_ind) {
				break;
			}

      // future 
      cur = blw;
      future = addition * pow(bf, (dh-ch)); // far
      offset += future; // far
		}
	}

	//case 3: d_abs_ind has h=1.
	if (dh == 1) {
		int trav_index = (d_abs_ind - 1) % bf;
		cur_node = cur_node->subtrees_.at(trav_index);
		return cur_node->data_;
	}

	return cur_node->data_;	
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


//Use DFS to sort
void Tree::sort() {
	int unq = 0;
	int alc_unq = 0;
	int size = 0;
	int bf = this->branching_factor_;
	int abs_ind = 1;
	int ef_ch = 1; //effective ch
	int h_ai = 0; //height of abs_ind
	if (this->root_ != NULL) {
		//as sort() always occurs after insert(), this registers the latent change.
		size++;
	} else {
		return;
	}
	int cap_efch = 1 + bf;
	int clo_efch = 1;
	const int mh = this->height(this->alc_unq_) + 1;
	while (ef_ch < mh) {
		//For every subindex of this->unq, create a path.
		vector<int> A;
		A.assign(ef_ch + 1, 0);
		//Reset or create A
		int i = ( (abs_ind - this->cap_less_one(h_ai)) / bf) + 1;
		int abv = -1;
		int cap = this->cap(ef_ch);
		int clo = cap_less_one(ef_ch);
		int clo_m1 = cap_less_one(ef_ch-1);
		for (int ch = ef_ch; ch > 0; ch--) {
			//If this is first in path, things not so straight forward.
			if (abv == -1) {
				A.at(ch) = abs_ind;
				if (ch == 1) {
					continue;
				} else {
					//if we are at end of path, exit or keep goind once more.  
					if (ch - 1 < 0) {
						if (ef_ch == h_ai) {
							abv = i + (0 - 1);
						} else {
							abv = (abv - clo / bf) + 1 + (0 - 1);
						}
					//If we are reach point in path which has height=ef_ch, exit. else keep going.
					} else {
						if (ef_ch == h_ai) {
							abv = i + (clo_m1 - 1);
						} else {
							abv = (abv - clo / bf) + 1 + (clo_m1 - 1);
						}
					}
				}
			//If this is not first in path, keep going.
			} else {
				A.at(ch) = abv;
				if (ch == 1) {
					continue;
				} else if (ch == 2) {
					abv = ( (abv - clo) / bf) + 1;
				} else {
					abv = ( (abv - clo) / bf ) + clo_m1;			
				} 
			}
			//
			cap -= pow(bf, ch);
			clo -= pow(bf, ch-1);
			clo_m1 -= pow(bf, ch-2);
		}	
		
		//For each ind in cap, regardless if its void, traverse its A.
		while (abs_ind < cap_efch) {
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
			//if final (current) node in path is void, don't count unq.
			//else count unq and alc_unq.
			int ins = (abs_ind - 1) % bf;
			if (ins < prev->subtrees_.size()) {
				if (get<1>(prev->subtrees_.at(ins)->data_) > -1) {
					get<1>(prev->subtrees_.at(ins)->data_) = abs_ind;				

					size++;
					unq = abs_ind;
					alc_unq = abs_ind;	

					//this->height(abs_ind) used throughout loop
					h_ai = std::floor( std::pow(abs_ind, (ef_ch/mh)) );
				} else if (get<1>(prev->subtrees_.at(ins)->data_) == -1) {
					alc_unq = abs_ind;
				}
			}
			//Transform A such that path from 0 to abs_ind is valid. 
			abs_ind++;
			A.at(ef_ch) = abs_ind;
			r(ef_ch, bf, A);
		}
		ef_ch++;
		//Used throughout this loop
		cap_efch += std::pow(bf, ef_ch);
		clo_efch += std::pow(bf, ef_ch-1);
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

int Tree::cap_less_one(int h) {
	//added
	if (h == 1) {
		return 1;
	} else if (h < 1) {
		//cout << "Error: Tree::cap_less_one(int): Invalid argument\n";
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

//Testing

char* istrue(bool val) {
    //Return 'true' if val is true, 'false' otherwise
    if (val == true) {
        char* ret = (char*)malloc(sizeof(char)*5);
        ret[0] = 't'; ret[1] = 'r'; ret[2] = 'u'; ret[3] = 'e'; ret[4] = '\0';
        return ret;
    }
    char* ret = (char*)malloc(sizeof(char)*6);
    ret[0] = 'f'; ret[1] = 'a'; ret[2] = 'l'; ret[3] = 's'; ret[4] = 'e'; ret[5] = '\0';
    return ret;
}

void begin_test() {
    cout << "\n\n---\t---\t---\n";
}

void end_test() {
    cout << "\n---\t---\t---\n\n";
}


void tree_test_code() {
	//Test insert
	begin_test();
	Tree tree0 = Tree();
	auto start = steady_clock::now();
	for (int num = 5; num < 13; num++) {
		tree0.insert(num);
	}
	auto end = steady_clock::now();
	auto duration = duration_cast<microseconds>(end - start);
	bool status = (duration.count() < 100); 
	cout << "This should read \'5 6 7 8 9 10 11 12\':\t";
	tree0.print();
	cout << "Benchmark status: " << istrue(status) << ".";
	end_test();
	
	//Test remove
	begin_test();
	start = steady_clock::now();
	tree0.remove();
	tree0.remove();
	tree0.remove();
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should read \'5 6 7 8 9\':\t"; tree0.print();
	cout << "Benchmark status: " << istrue(status) << ".";
	end_test();
	
	//test convert()
	begin_test();
	vector<double> array; array.reserve(tree0.get_size()); 
	start = steady_clock::now();
	array = tree0.convert();
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end-start); 
	status = (duration.count() < 100);
	int index = 0;
	cout << "This should read \'5 6 7 8 9\':\t";
	for (auto& index : array) {
		cout << index << " ";
	} cout << "\n";
    cout << "Benchmark status: " << istrue(status) << ".";
    end_test();
		
	//Test Tree::operator=
    begin_test();
	Tree tree1 = Tree();
    start = steady_clock::now();
	tree1 = tree0; 
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This should read \'5 6 7 8 9\':\t"; tree1.print();
    cout << "Benchmark status: " << istrue(status) << ".";
    end_test();
    
	
	//Test convert() 
    begin_test();
    start = steady_clock::now();
	vector<double> array1; array1 = tree1.convert();
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This should read \'5 6 7 8 9\':\t";
	for (auto& index : array1) {
		cout << index << " ";
	} cout << "\n";
    cout << "Benchmark status: " << istrue(status) << ".";  
    end_test();
	
	//Test insert_here
    begin_test();
    start = steady_clock::now();
	tree1.insert((double)15, 2, 2);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This (the unq_ of the tree, which is " << tree1.get_unq() << ") should be \'7\'.\n";
	cout << "This (the alc_unq_ of the tree, which is " << tree1.get_alc_unq() << ") should be \'7\'.\n";
    cout << "Benchmark status: " << istrue(status) << ".";

    cout << "\n";

	cout << "Also, this should read \'5 6 7 8 9 15\':\t";
	tree1.print(); 
    start = steady_clock::now();
	tree1.insert((double)20, 2, 7);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This (the unq_ of the tree, which is " << tree1.get_unq() << ") should be \'12\'.\n";
	cout << "This (the alc_unq_ of the tree, which is " << tree1.get_alc_unq() << ") should be \'13\'.\n";
    cout << "Benchmark status: " << istrue(status) << ".";

    cout << "\n";

	cout << "This should read \'5 6 7 8 9 15 20\':\t"; tree1.print();
	Tree tree2 = Tree();
    start = steady_clock::now();
	tree2.insert(1);
	tree2.insert((double)1, 1, 2);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This should read \'1 1\':\t"; tree2.print();
    cout << "Benchmark status: " << istrue(status) << ".";
    end_test();
	
	//test remove_here
    begin_test();
    start = steady_clock::now();
	tree2.remove(1, 2);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This should read \'1\':\t"; tree2.print(); 
    cout << "Benchmark status: " << istrue(status) << ".";
    end_test();
	tree2.insert((double)1, 1, 2);
	
	//Test alloc_lvl
    begin_test();
	cout << "What is this (the unq_ of the tree)? It\'s " << tree2.get_unq() << "\n";
	cout << "alc_unq_ should read \'4\':\t";
    start = steady_clock::now();
    cout << tree2.get_alc_unq();
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
    cout << "\nBenchmark status: " << istrue(status) << ".";

    cout << "\n";

    start = steady_clock::now();
	tree2.alloc_lvl();
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
    cout << "Benchmark status: " << istrue(status) << ".\n";
	cout << "What is this (the unq_ of the tree)? It\'s " << tree2.get_unq() << "\n";
	cout << "alc_unq_ should read \'13\':\t" << tree2.get_alc_unq() << "\n";
    start = steady_clock::now();
	tree2.alloc_by_bal();
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
    cout << "Benchmark status: " << istrue(status) << ".\n";
	tree2.alloc_lvl();
	cout << "This should read \'1 1\':\t"; tree2.print();
    end_test();
	
	//Test insert_here error message for creating a disconnected graph
	cout << "This should read \'Error: attempt to create a disconnected graph\':\t";
	tree2.insert(1, 2, 3);
	tree2.print();
	
	//Test branching factor
    begin_test();
	Tree tree3 = Tree();
	tree3.set_branching_factor(4);
    start = steady_clock::now();
	for (int index = 44; index < 59; index++) {
		tree3.insert(index);
	}
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This should read \'44 45\', 44-58.\t"; tree3.print();
    cout << "Benchmark status: " << istrue(status) << ".";
    end_test();
	
	//Test assignment
	Tree tree4 = Tree(tree3);
	cout << "This should also read \'44 45\', 44-58.\t"; tree4.print();
	
	//test remove_here
    begin_test();
	cout << "Should read \'44 45 46 47 48 49 50 51 52 53 55 56 57 58\':\t";
    start = steady_clock::now();
	tree4.remove(2, 5);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	tree4.print();
    cout << "Benchmark status: " << istrue(status) << ".";

    cout << "\n";

	cout << "Now it should read \'44 45 46 48 49 50 51 52 53 55 56\':\t";
    start = steady_clock::now();
	tree4.remove(1, 2);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	tree4.print();
    cout << "Benchmark status: " << istrue(status) << ".";

    cout << "\n";

	cout << "Now it should read \'44 46 48 53 55 56\':\t";
    start = steady_clock::now();
	tree4.remove(1, 0);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	tree4.print();
    cout << "Benchmark status: " << istrue(status) << ".";

	//test insert_here
    cout << "\n";

	cout << "Now it should read \'44 1.23 46 48 53 55 56\':\t";
    start = steady_clock::now();
	tree4.insert(1.23, 1, 0);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	tree4.print();
    cout << "Benchmark status: " << istrue(status) << ".";

    cout << "\n";

	cout << "Now it should read \'44 1.23 46 48 4.56 53 55 56\':\t";
    start = steady_clock::now();
	tree4.insert(4.56, 2, 0);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	tree4.print();
    cout << "Benchmark status: " << istrue(status) << ".";

	
	//test remove
	cout << "Now it should read \'44 1.23 46 48 4.56 55 56\':\t";
    start = steady_clock::now();
	tree4.remove(2, 4);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
    tree4.print();
    cout << "Benchmark status: " << istrue(status) << ".\n";
	
	//test insert
	cout << "Now it should read \'44 1.23 46 7.89 48 4.56 55 56\':\t";
	start = steady_clock::now();
	tree4.insert(7.89);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	tree4.print();
	cout << "Benchmark status: " << istrue(status) << ".";
	end_test();

	//test balance
    begin_test();
	cout << "The alc_unq_ currently is \'" << tree4.get_alc_unq() << "\'; it should be \'" << 13 << "\'\n";
    start = steady_clock::now();
	tree4.alloc_by_bal();
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
    cout << "Benchmark status: " << istrue(status) << ".\n";
	cout << "The alc_unq_ now is \'" << tree4.get_alc_unq() << "\'; it should be \'" << 21 << "\'\n";
	tree4.print();
    end_test();
	//test DFS
	Tree tree5 = Tree();
	for (int num = 0; num < 40; num++) {
		tree5.insert(num);
	}
		
	pair<double, int> main_pair;
    start = steady_clock::now();
	main_pair = tree5.NDFS(14);
    end = steady_clock::now();
    duration = duration_cast<microseconds>(end - start);
    status = (duration.count() < 100);
	cout << "This should be (14, 14):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
    cout << "Benchmark status: " << istrue(status) << ".";

    start = steady_clock::now();
	main_pair = tree5.NDFS(38);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (38, 38):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	start = steady_clock::now();
	main_pair = tree5.NDFS(26);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (26, 26):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	start = steady_clock::now();
	main_pair = tree5.NDFS(2);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (2, 2):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	start = steady_clock::now();
	main_pair = tree5.NDFS(0);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (0, 0):\n\n";
	cout << "("<<get<0>(main_pair)<<", "<<get<1>(main_pair)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	//test DFS
		
	Tree tree6 = Tree();
	for (int num = 0; num < 100; num++) {
		tree6.insert(num);
	}
	pair<double, int> alt_pair;
	alt_pair = tree6.NDFS(75);
	cout << "This should be (75, 75):\n\n";
	cout << "("<<get<0>(alt_pair)<<", "<<get<1>(alt_pair)<<")\n";
	alt_pair = tree6.NDFS(99);
	cout << "This should be (99, 99):\n\n";
	cout << "("<<get<0>(alt_pair)<<", "<<get<1>(alt_pair)<<")\n";

	//test DFS
	Tree tree7 = Tree();
	for (int num = 0; num < 150; num++) {
		tree7.insert(num);
	}
	pair<double, int> pair2;
	start = steady_clock::now();
	pair2 = tree7.NDFS(125);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (125, 125):\n\n";
	cout << "("<<get<0>(pair2)<<", "<<get<1>(pair2)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	start = steady_clock::now();
	pair2 = tree7.NDFS(149);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (149, 149):\n\n";
	cout << "("<<get<0>(pair2)<<", "<<get<1>(pair2)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	//test DFS
	Tree tree8 = Tree();
	for (int num = 0; num < 300; num++) {
		tree8.insert(num);
	}
	pair<double, int> pair3;
	start = steady_clock::now();
	pair3 = tree8.NDFS(225);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (225, 225):\n\n";
	cout << "("<<get<0>(pair3)<<", "<<get<1>(pair3)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	start = steady_clock::now();
	pair3 = tree8.NDFS(299);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (299, 299):\n\n";
	cout << "("<<get<0>(pair3)<<", "<<get<1>(pair3)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";

	cout << "This should be (999, 999): \n\n";
	Tree tree9 = Tree();
	for (int num = 0; num < 1000; num++) {
		tree9.insert(num);
	}
	start = steady_clock::now();
	pair3 = tree9.NDFS(998);
	end = steady_clock::now();
	duration = duration_cast<microseconds>(end - start);
	status = (duration.count() < 100);
	cout << "This should be (998, 998):\n\n";
	cout << "("<<get<0>(pair3)<<", "<<get<1>(pair3)<<")\n";
	cout << "Benchmark status: " << istrue(status) << ".";
	end_test();
}
