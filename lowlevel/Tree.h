//Header file for a simple, directed, and connected variable-node tree.
//
//Default branching factor of three; removal means deleting the treenode and thus all subtrees of it. 
//Also, cannot insert into a location that has data stored - this data must be removed.
//Will raise an error and exit should this behavior be encountered.
//
//This tree
//
//By Lorenz Hunter
//Autumn 2025

#ifndef TREE_H
#define TREE_H

#include "TreeNode.h"
#include <queue>
#include <utility>
#include <array>
#include <algorithm>
#include <vector>
using std::get;
using std::pair;
using std::queue;
using std::array;
using std::none_of;
using std::vector;

//Note: 'this->branching_factor_' is constant in all cases in which tree is not empty.
class Tree {
	private:
		TreeNode* root_;
		int branching_factor_ = 0;
		int size_ = 0;
		int unq_ = 0;
		int alc_unq_ = 0;
		int fv_ = 0; //first void index
		/*note: unq_, alc_unq, and fv_ are all 1 ahead of what they denote.*/
		void print_helper();
		void set_size(int data);
		void increment_size();
		void decrement_size();
		void increment_unq();
		void set_unq(int data);
		void set_alc_unq(int data);
		void increment_alc_unq();
		void decrement_alc_unq();
		int cap_less_one(int h);
		int cap(int h);
		void sort();
		void insert(void* blank);
		bool is_alloc_bal(); 
		int alc_ht();
		int count_alc();
		void r(int n, int bf, vector<int>& A);

	public:
		//rule of three
		Tree();
		Tree(const Tree& other);
		~Tree();
		Tree& operator=(Tree& other);
		//function
		void insert(double data);
		void print();
		void remove();
		vector<double> convert();
		void insert(double data, int desired_depth, int desired_d);
		void remove(int desired_depth, int desired_d);
		bool is_balanced();
		bool is_balanced(int h);
		void alloc_by_bal();
		void alloc_lvl();
//		void alloc();
		int height();	
		int height(int d_abs_ind);
		int unalc_ht();
		pair<double, int> DFS(int abs_index);
		pair<double, int> NDFS(int abs_index);
		TreeNode* DFST(int abs_index);
		pair<double, int> BFS(int abs_index);

		int get_branching_factor();
		int get_size();
		int get_unq();

		int get_alc_unq();
		void set_branching_factor(int data);
};

void tree_test_code();
bool is_neg_one(int i);
bool g_thn_zro(int i);
bool g_thn_neg_one(int i);

#endif
