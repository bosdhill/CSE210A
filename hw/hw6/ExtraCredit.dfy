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
	e
}

//as you write optimize this will become unproved
//you must write proof code so that Dafny can prove this
 method optimizeCorrect(e:Exp, s:map<string, int>)
ensures eval(e,s) == eval(optimize(e), s)
{

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