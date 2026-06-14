//TreeNode header file for arbitrarily many number of subtrees
//Lorenz Hunter
//Spring 2025

#ifndef TREENODE_H
#define TREENODE_H
#include <vector>
#include <utility>
using std::pair;
using std::get;
using std::vector;

class TreeNode {
	public:
		pair<double, int> data_;
		vector<TreeNode*> subtrees_;
		TreeNode(pair<double, int> data);
		~TreeNode();
};

#endif
