datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun preorderold Lf = []
| preorderold (Br(v, t1, t2)) = [v] @ preorderold t1 @ preorderold t2;

fun inorderold Lf = []
| inorderold (Br(v, t1, t2)) = inorderold t1 @ [v] @ inorderold t2;

fun postorderold Lf = []
| postorderold (Br(v, t1, t2)) = postorderold t1 @ postorderold t2 @ [v];


(* Need to fix quadratic time on badly unbalanced trees - add an accumulator to rid of append calls (expensive) - cons :: better *)
fun preorder Lf = vs
| preorder (Br(v, t1, t2), vs) = v::(preorder(t1, preorder (t2, vs)));

fun inorder Lf = vs
| inorder (Br(v, t1, t2), vs) = inorder(t1, v :: inorder (t2, vs));

fun postorder Lf = vs
| postorder (Br(v, t1, t2), vs) = postorder(t1, postorder(t2, vs) :: v);

val birnam = Br(1, Br(2, Lf, Br(3,Br(4,Lf,Lf), Lf)), Lf);