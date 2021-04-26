type key = string;
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert(key, LEAF) = TREE(LEAF, key, LEAF)
  | insert(key, TREE(l, k, r)) = 
        if key < k
           then TREE(insert(key, l), k, r)
        else if key > k
           then TREE(l, k, insert(key, r))
        else
           TREE(l, key, r)

fun member(key, LEAF) = false
  | member(key, TREE(l, k, r)) = 
        if key < k
            then member(key, l)
        else if key > k
            then member(key, r)
        else
            true

fun test_insert_member() = 
    let val tree_test = insert("3", insert("5", insert("1", LEAF)))
    in
      member("1", tree_test) andalso member("3", tree_test) andalso member("5", tree_test)
    end

val test_result = test_insert_member()
