datatype Tree<T> = Leaf | Node(Tree<T>, Tree<T>, T)
datatype List<T> = Nil | Cons(T, List<T>)


function flatten<T>(tree:Tree<T>):List<T>
{
	match(tree)
       case Leaf => Nil
       case Node(T1, T2, element) => Cons(element, append(flatten(T1), flatten(T2)))
}

function append<T>(xs:List<T>, ys:List<T>):List<T>
{
	match(xs)
		case Nil => ys
		case Cons(x, xs') => Cons(x, append(xs', ys))
}

function treeContains<T>(tree:Tree<T>, element:T):bool
{
    match(tree)
        case Leaf => false
        case Node(T1, T2, element') => element' == element || treeContains(T1, element) || treeContains(T2, element)
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

ghost method listContains'<T>(xs:List<T>, ys:List<T>, element:T)
ensures  listContains(append(xs, ys), element) <==> listContains(xs, element) || listContains(ys, element)
{
    match(xs) {
        case Nil => { }
        case Cons(x, xs') => {
            listContains'(xs', ys, element);
            assert (
                listContains(append(xs, ys), element)
                == (x == element || listContains(append(xs', ys), element))
                == (x == element || listContains(xs', element) || listContains(ys, element))
                == listContains(xs, element) || listContains(ys, element)
            );
        }
    }

}

lemma sameElements<T>(tree:Tree<T>, element:T)
ensures treeContains(tree, element) <==> listContains(flatten(tree), element)
{
    match(tree)
        case Leaf => { }
        case Node(T1, T2, element') => {
            listContains'(flatten(T1), flatten(T2), element);
            assert(
                treeContains(tree, element)
                == (element' == element || treeContains(T1, element) || treeContains(T2, element))
                == (element' == element || listContains(flatten(T1), element) || listContains(flatten(T2), element))
                == (element' == element || listContains(append(flatten(T1), flatten(T2)), element))
                == listContains(flatten(tree), element)
            );
        }
}
