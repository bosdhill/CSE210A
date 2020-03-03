method double(x:int) returns (y:int)
requires x >= 2;
ensures y >= x*2;
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
	var k := 0;
	if(3<4){
		i := i+1;
	}else {
		i := i-1;
	}
	assert(i==1);
	while(i<a.Length && a[i] != x)
	//decreases a.Length - i;
	invariant i <= a.Length;
	invariant forall j:: (0 <= j < i) ==> a[j] != x;
	{
		i := i+1;
		k := k+1;
	}
	assert(k==a.Length);
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


function method reverse<T>(xs:List<T>):List<T>
ensures length(xs) == length(reverse(xs))
//ensures reverse(reverse(xs)) == xs
//ensures reverse(append(xs, ys)) == append(reverse(ys), reverse(xs))
{
	match(xs)
		case Nil => Nil
		case Cons(y, ys) => append(reverse(ys), Cons(y, Nil))
}

function helperReverse<T>(xs:List<T>, acc:List<T>):List<T>
//ensures append(reverse(xs), ys) == helperReverse(xs, ys);
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

method helperReverseOK<T>(xs:List<T>, acc:List<T>)
ensures append(reverse(xs), acc) == helperReverse(xs, acc);
{
	match(xs)
		case Nil => { }
		case Cons(y, ys) => {
			helperReverseOK(ys, Cons(y, acc));
			appendAssoc(reverse(ys), Cons(y, Nil), acc);

			assert( append(reverse(xs), acc)
				== append(reverse( Cons(y, ys) ), acc)
				== append( append( reverse(ys), Cons(y, Nil)), acc)
				== append( reverse(ys), append( Cons(y, Nil), acc) )
				== append( reverse(ys), Cons(y, acc) )
				== helperReverse(ys, Cons(y, acc))
				== helperReverse(Cons(y, ys), acc)
				== helperReverse(ys, Cons(y, acc))
			);
		}
}

method fastReverseOK<T>(xs:List<T>)
ensures fastReverse(xs) == reverse(xs)
{
	helperReverseOK(xs, Nil);
}

function method append<T>(xs:List<T>, ys:List<T>):List<T>
ensures (xs == Nil) ==> (append(xs, ys) == ys)
ensures (ys == Nil) ==> (append(xs, ys) == xs)
ensures length(append(xs, ys)) == length(xs) + length(ys)
{
	match(xs)
		case Nil => ys
		case Cons(z, zs) => Cons(z, append(zs, ys))
}

method lengthDistributesAppend<T>(xs:List<T>, ys:List<T>)
ensures length(append(xs, ys)) == length(xs)+length(ys)
{
	match(xs){
		case Nil => {
		/*
			assert( length(append(xs, ys))
				== length(append(Nil, ys))
				== length(ys));

			assert( length(xs) + length(ys)
				== length<T>(Nil) + length(ys)
				== 0 + length(ys)
				== length(ys));
				*/
		}
		case Cons(z, zs) => {
		/*
			assert( length(append(xs, ys))
				== length(append(Cons(z, zs), ys))
				== length(Cons(z, append(zs, ys)))
				== 1 + length(append(zs, ys)) );

			assert( length(xs) + length(ys)
				== length(Cons(z,zs)) + length(ys)
				== (1 + length(zs)) + length(ys)
				== 1 + (length(zs) + length(ys)) );
				*/
		}
	}
}

method appendNil<T>(xs:List<T>)
ensures append(xs, Nil) == xs;
{
	match(xs){
		case Nil => {

		}
		case Cons(z, zs) => {
			appendNil(zs);
			/*
			assert( append( Cons(z,zs), Nil)
				== Cons(z, append(zs, Nil))
				== Cons(z, zs)
				);
				*/
		}
	}
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

method emma<T>(xs:List<T>, ys:List<T>)
ensures reverse(append(xs, ys)) == append(reverse(ys), reverse(xs))
{
	match(xs){
		case Nil =>{
		/*
			assert( reverse(append(Nil, ys))
				== reverse(ys));
				*/
			appendNil(reverse(ys));
			/*
			assert( append(reverse(ys), reverse(Nil))
				== append(reverse(ys), Nil)
				== reverse(ys));
				*/


			calc{ reverse(append(Nil, ys))
				== reverse(ys); }

			appendNil(reverse(ys));

			calc{ append(reverse(ys), reverse(Nil))
				== append(reverse(ys), Nil)
				==  reverse(ys); }
		}
		case Cons(z, zs) => {

			emma(zs, ys);
			/*
			calc{ reverse(append(xs, ys))
				== reverse(append(Cons(z,zs), ys))
				== reverse(Cons(z, append(zs, ys)))
				== append(reverse(append(zs, ys)), Cons(z, Nil))
				== append(append(reverse(ys), reverse(zs)), Cons(z, Nil))
				;}

			calc{ append(reverse(ys), reverse(xs))
				== append(reverse(ys), reverse(Cons(z,zs)))
				==  append(reverse(ys), append(reverse(zs), Cons(z, Nil)))
				;}
				*/

			appendAssoc( reverse(ys), reverse(zs), Cons(z, Nil));
		}
	}

}
