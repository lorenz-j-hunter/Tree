# B-Tree
Tree with user-selected branching factor. Uses DFS as its primary search method.
---
##### Details
The tree is kept so that there are no void indices past the last full node. In practice,
if there exists a node for which all of its first-order children are void, the subtrees
vector is deallocated.
---
##### Nomenclature
<dl>
  <dt>Definition list</dt>
  <dt>bf</dt>
  <dd>The branching factor.</dd>
  <dt>ch</dt>
  <dd>Current height.</dd>
  <dt>dh</dt>
  <dd>Desired height></dt>
  <dt><strong>void node</strong></dt>
  <dd>A node which has been allocated but does not store meaningful data. These are represented by 
  <code>TreeNode</code>s with <code>data_</code>=(0, -1).
  A void node cannot have children.</dd>
  <dt><strong>null node</strong></dt>
  <dd>This is a concept raised when a search does not find the desired node in the tree.
  It is not a node, and so it's not an allocated node.
  It is also used as a return value when a function fails silently. 
  It is commonly denoted by <code>null_p</code> (null_pointer) pairs or TreeNodes throughout the program.</dd>
  <dt><code>cap</code></dt>
  <dd>The capacity of the tree. For a tree with a branching factor of 3 and a height of 4, 
  the capacity is 3^(4) + 3^(3) + 3^(2) + 3^(1) + 1 = 121.</dd>
  <dt><code>cap_less_one</code>, <code>clo</code></dt>
  <dd>The capacity of a tree for which height is 1 minus the height in question.
  For a tree of branching factor 3 and height 4, the `clo` is 3^(4-1) + 3^(3-1) + 3^(2-1) + 1 = 40.</dd>
  <dt><code>this->unq_</code></dt>
  <dd>Refers to the last full index, plus one. Suppose that a tree of height 3 has this->size_=27; it's full.
  this->unq_ is then 28. Now suppose we have a tree of height 3 but this->size_=4; it's not full. The last full
  index has an index of 15 (between cap_less_one(3) of 13 and cap(3) of 40). Then this->unq_=16.</dd>
  <dt><code>this->alc_unq_</code></dt>
  <dd>Like unq_, but refers to <em>allocated</em> nodes. this->alc_unq_ >= this->unq_ in all circumstances.</dd>
  <dt><code>this->fv_</code></dt>
  <dd>Refers to the first void node by index in the tree.</dd>
  <dt><code>d</code></dt>
  <dd>Refers to the "distance" of a node in question from the beginning of its level.
  The <code>d</code> = <code>d_abs_ind</code>-<code>cap_less_one_</code>.</dd>
</dl>
---

##### Problems and current state (6-30-26)
There is a semantic bug in `Tree::remove(int desired_depth, int desired_d)`. Details inside.