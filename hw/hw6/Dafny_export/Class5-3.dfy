method double(x:int) returns (y:int)
requires x >= 2;
ensures y > x*2;
{
	y := x+x;
}

method fib(x:int) returns (y:int)
requires x>=1;
ensures y>=x;
{
	if(x==1 || x==2){
		y:=x;
	}else{
		var a:=fib(x-1);
		var b:=fib(x-2);
		y:=a+b;
	}
}

method mul(x:int, y:int) returns (s:int)
requires x>=0;
ensures s == x*y;
{
	s := 0;
	var c := 0;
	while(c != x)
	invariant s==c*y && c<=x;
	decreases x-c;
	{
		c := c+1;
		s := s+y;
	}
}

method find<T(==)>(x:T, a:array<T>) returns (r:int)
requires a != null;
ensures 0 <= r && r <= a.Length;
ensures r < a.Length ==> a[r] == x;
ensures forall j:: (0 <= j < r) ==> a[j] != x;
{
	var i := 0;
	while(i<a.Length && a[i] != x)
	//decreases a.Length - i;
	invariant i <= a.Length;
	invariant forall j:: (0 <= j < i) ==> a[j] != x;
	{
		i := i+1;
	}
	r := i;
}

datatype Tree<T> = Leaf(T) | Node(Tree<T>, Tree<T>)

function mirror(t:Tree):Tree
ensures true;
requires true;
{
	match(t)
		case Leaf(x) => Leaf(x)
		case Node(t1, t2) => Node(mirror(t2), mirror(t1))
}

datatype List<T> = Nil | Cons(T, List<T>)

function head<T>(xs:List<T>):T
requires xs != Nil;
{
	match(xs)
		case Cons(y, ys) => y
}

function length<T>(xs:List<T>):int
ensures length(xs) >= 0;
//ensures length(xs) == length(reverse(xs))
ensures (xs == Nil) ==> (length(xs) == 0)
{
	match(xs)
		case Nil => 0
		case Cons(y, ys) => 1 + length(ys)
}

function elementOf<T>(elm:T, xs:List<T>):bool
{
	match(xs)
		case Nil => false
		case Cons(y, ys) => y==elm || elementOf(elm, ys)
}

function append<T>(xs:List<T>, ys:List<T>):List<T>
ensures (xs == Nil) ==> (append(xs, ys) == ys)
ensures (ys == Nil) ==> (append(xs, ys) == xs)
ensures length(append(xs, ys)) == length(xs) + length(ys)
{
	match(xs)
		case Nil => ys
		case Cons(z, zs) => Cons(z, append(zs, ys))
}

function reverse<T>(xs:List<T>):List<T>
ensures length(xs) == length(reverse(xs))
//ensures reverse(reverse(xs)) == xs
//ensures reverse(append(xs, ys)) == append(reverse(ys), reverse(xs))
{
	match(xs)
		case Nil => Nil
		case Cons(y, ys) => append(reverse(ys), Cons(y, Nil))
}

function helperReverse<T>(xs:List<T>, acc:List<T>):List<T>
//ensures append(reverse(xs), ys) == betterReverse(xs, ys);
{
	match(xs)
		case Nil => acc
		case Cons(y, ys) => helperReverse(ys, Cons(y, acc))
}

function fastReverse<T>(xs:List<T>):List<T>
//ensures fastReverse(xs) == reverse(xs);
{
	helperReverse(xs, Nil)
}


method emma<T>(xs:List<T>, ys:List<T>)
ensures reverse(append(xs, ys)) == append(reverse(ys), reverse(xs)) 
{
	match(xs){
		case Nil =>{ }
		case Cons(x, xs) => {
			match(ys){
				case Nil => { }
				case Cons(y, ys) => {
					//assert( reverse(append(xs, ys)) == append(reverse(ys), reverse(xs)) );
				}
			}
		}
	}

}
