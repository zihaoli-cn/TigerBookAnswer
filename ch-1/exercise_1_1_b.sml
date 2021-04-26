type key = string;
datatype 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

val empty = LEAF

fun insert(LEAF, key_in, value) = TREE(LEAF, key_in, value, LEAF)
  | insert(TREE(l, k, v, r), key_in, value) = 
        if key_in < k
           then TREE(insert(l, key_in, value), k, v, r)
        else if key_in > k
           then TREE(l, k, v, insert(r, key_in, value))
        else
           TREE(l, key_in, value, r)

exception NotFound;

fun lookup(LEAF, key_in) = raise NotFound
  | lookup(TREE(l, k, v, r), key_in) = 
        if key_in < k
            then lookup(l, key_in)
        else if key_in > k
            then lookup(r, key_in)
        else
            v

fun test_insert_lookup() = 
    let val tree_test = insert(insert(insert(LEAF, "1", 1), "2", 2), "3", 3)
    in
        (lookup(tree_test, "1") = 1) andalso
        (lookup(tree_test, "2") = 2) andalso
        (lookup(tree_test, "3") = 3) andalso
        (lookup(tree_test, "4") = 4) handle NotFound => true
    end

val test_result = test_insert_lookup()
