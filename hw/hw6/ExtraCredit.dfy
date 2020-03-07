datatype Exp = Const(int) | Var(string) | Plus(Exp, Exp) |  Mult(Exp, Exp)

function eval(e:Exp, store:map<string, int>):int
{
	match(e)
		case Const(n) => n
		case Var(s) => if(s in store) then store[s] else -1
		case Plus(e1, e2) => eval(e1, store) + eval(e2, store)
		case Mult(e1, e2) => eval(e1, store) * eval(e2, store)
}

//fill this function in to make optimizeFeatures work
function optimize(e:Exp):Exp
{
	match(e)
		case Mult(x, y) => (
			match(optimize(x), optimize(y))
				case (x', y') =>
					if (x' == Const(0) || y' == Const(0))
					then Const(0)
					else
					if y' == Const(1)
					then x'
					else
					if x' == Const(1)
					then y'
					else matchMultCase(x', y')
			)
 		case Plus(x, y) => (
			match(optimize(x), optimize(y))
				case (x', y') =>
					if (x' == Const(0))
					then y'
					else
					if (y' == Const(0))
					then x'
					else matchPlusCase(x', y')
		 )
		case Var(x) => Var(x)
		case Const(x) => Const(x)
}

function matchPlusCase(x':Exp, y':Exp):Exp
{
	match(x', y')
		case (Const(x), Const(y)) => Const(x + y)
		case (Const(x), Var(y)) => Plus(x', y')
		case (Const(x), Plus(x'', y'')) => Plus(x', y')
		case (Const(x), Mult(x'', y'')) => Plus(x', y')
		case (Var(x''), y) => Plus(x', y')
		case (Plus(x'', y''), y) => Plus(x', y')
		case (Mult(x'', y''), y) => Plus(x', y')
}

function matchMultCase(x':Exp, y':Exp):Exp
{
	match(x', y') {
		case (Const(x), Const(y)) => Const(x * y)
		case (Const(x), Plus(x'', y'')) => Mult(x', y')
		case (Const(x), Mult(x'', y''')) => Mult(x', y')
		case (Const(x), Var(y'')) => Mult(x', y')
		case (Var(x''), y) => Mult(x', y')
		case (Plus(x'', y''), y) => Mult(x',y')
		case (Mult(x'', y''), y) => Mult(x',y')
		case (Plus(x''', y''')) => Mult(x', y')
	}
}

//as you write optimize this will become unproved
//you must write proof code so that Dafny can prove this
 method optimizeCorrect(e:Exp, s:map<string, int>)
ensures eval(e,s) == eval(optimize(e), s)
decreases e
{
	match(e)
		case Mult(x, y) => {
			optimizeCorrect(y, s);
			optimizeCorrect(x, s);
		}
		case Plus(x, y) => {
			optimizeCorrect(y, s);
			optimizeCorrect(x, s);
		}
		case Var(x) => {}
		case Const(x) => {}
}

method optimizeFeatures()
{

	assert( optimize(Mult(Var("x"), Const(0))) == Const(0) );
	assert( optimize(Mult(Var("x"), Const(1))) == Var("x") );
	assert( optimize(Mult(Const(0), Var("x"))) == Const(0) );
	assert( optimize(Mult(Const(1), Var("x"))) == Var("x") );

	assert( optimize(Plus(Const(0), Var("x"))) == Var("x") );
	assert( optimize(Plus(Var("x"), Const(0))) == Var("x") );

	assert( optimize(Plus(Const(3),Const(4))) == Const(7) );
	assert( optimize(Mult(Const(3),Const(4))) == Const(12) );


	assert( optimize(Plus(Plus(Var("x"), Var("y")), Const(0))) == Plus(Var("x"), Var("y")) );
	
}