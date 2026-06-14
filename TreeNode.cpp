//TreeNode.cpp
//By Lorenz Hunter
//Spring 2025

#include "TreeNode.h"

TreeNode::TreeNode(pair<double, int> data) {
	get<0>(this->data_) = get<0>(data);
	get<1>(this->data_) = get<1>(data);
	this->subtrees_ = vector<TreeNode*>();	
}

TreeNode::~TreeNode() {
}

