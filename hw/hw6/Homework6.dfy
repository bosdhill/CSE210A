datatype Tree<T> = Leaf | Node(Tree<T>, Tree<T>, T)
datatype List<T> = Nil | Cons(T, List<T>)


function flatten<T>(tree:Tree<T>):List<T>
{
	match(tree)
       case Leaf => Nil
       case Node(T1, T2, T) => Cons(T, append(flatten(T1), flatten(T2)))
}

function append<T>(xs:List<T>, ys:List<T>):List<T>
{
	match(xs)
		case Nil => ys
		case Cons(xsFirst, xsRest) => Cons(xsFirst, append(xsRest, ys))
}

function treeContains<T>(tree:Tree<T>, element:T):bool
{
    match(tree)
        case Leaf => false
        case Node(T1, T2, T) => T == element || treeContains(T1, element) || treeContains(T2, element)
}

function listContains<T>(xs:List<T>, element:T):bool
{
    match(xs)
        case Nil => false
        case Cons(x, xs') => x == element || listContains(xs', element)
}

method appendAssoc<T>(xs:List<T>, ys:List<T>, zs:List<T>)
ensures append(append(xs, ys), zs) == append(xs, append(ys, zs))
{

	match(xs)
		case Nil => { }
		case Cons(w, ws) => {
			appendAssoc(ws, ys, zs);
			calc{ append(append(xs, ys), zs)
				== append(append(Cons(w, ws), ys), zs)
				== append(Cons(w, append(ws,ys)), zs)
				== Cons(w, append(append(ws, ys), zs))
			;}
		}

}

lemma sameElements<T>(tree:Tree<T>, element:T)
ensures treeContains(tree, element) == listContains(flatten(tree), element)
{
    match(tree)
        case Leaf => { }
        case Node(T1, T2, T) => {
            assert(
                treeContains(tree, element)
                == (T == element || treeContains(T1, element) || treeContains(T2, element))
                == (T == element || listContains(flatten(T1), element) || listContains(flatten(T2), element))
                == (T == element || listContains(append(flatten(T1), flatten(T2)), element))
                // == listContains(flatten(tree), element)
            );
        }
}