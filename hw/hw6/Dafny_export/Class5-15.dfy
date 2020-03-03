datatype Exp = Const(int) | Plus(Exp, Exp)
datatype Instruction = Push(int) | Add
datatype List<T> = Nil | Cons(T, List<T>)
datatype ErrorOrOk<T> = Error | OK(T)

function eval(e:Exp):int
{
	match(e)
		case Const(n) => n
		case Plus(e1, e2) => eval(e1) - eval(e2)
}

function run(program:List<Instruction>, stack:List<int>):ErrorOrOk<int>
{
	match(program)
		case Nil => if stack == Nil || tail(stack) != Nil
						then Error
						else OK(head(stack))
		case Cons(ins, rest) => match(ins)
			case Push(n) => run(rest, Cons(n, stack))
			case Add =>
				if(stack == Nil || tail(stack) == Nil)
				then Error
				else run(rest, Cons(head(tail(stack)) - head(stack), tail(tail(stack))))
}

function compile(e:Exp):List<Instruction>
{
	match(e)
		case Const(n) => Cons(Push(n), Nil)
		case Plus(e1, e2) => append(append(compile(e1), compile(e2)), Cons(Add, Nil))
							//append(compile(e1), append(compile(e2), Cons(Add, Nil)))
}

// Proofs
ghost method compileOK(e:Exp)
ensures run(compile(e), Nil) == OK(eval(e))
{
	compileOK'(e, Nil, Nil);
	appendNil(compile(e));
}

ghost method compileOK'(e:Exp, p:List<Instruction>, stack:List<int> )
ensures run(append(compile(e), p), stack) == run(p, Cons(eval(e), stack));
{
	match(e){
		case Const(n) => { }
		case Plus(e1, e2) =>{
			calc{ run( append( compile( Plus(e1, e2) ), p), stack);
					== run( append( append( append( compile(e1), compile(e2) ), Cons(Add, Nil) ), p), stack);
					== { appendAssoc(append(compile(e1), compile(e2)), Cons(Add, Nil), p);
						appendAssoc(compile(e1), compile(e2), append(Cons(Add, Nil), p)); }
						run( append( compile(e1), append( compile(e2), append( Cons(Add, Nil), p))), stack);
					== { compileOK'(e1, append(append(compile(e2), Cons(Add, Nil)), p), stack); }
						run( append (compile(e2), append( Cons(Add, Nil), p)), Cons(eval(e1), stack));
					== { compileOK'(e2, append(Cons(Add, Nil), p), Cons(eval(e1), stack)); }
						run( append( Cons(Add, Nil), p), Cons(eval(e2), Cons(eval(e1), stack)) );
					== run( p, Cons(eval(e1) - eval(e2), stack) );
			}
		}
	}

}


// List helper code
function append<T>(xs:List<T>, ys:List<T>):List<T>
{
	match(xs)
		case Nil => ys
		case Cons(xsFirst, xsRest) =>  Cons(xsFirst, append(xsRest, ys))
}

function length<T>(xs:List<T>):int
ensures xs == Nil ==> length(xs) == 0
{
	match(xs)
		case Nil => 0
		case Cons(y, ys) => 1 + length(ys)
}


function head<T>(xs:List<T>):T
requires xs != Nil
{
	match(xs)
		case Cons(y, ys) => y
}

function tail<T>(xs:List<T>):List<T>
requires xs != Nil
ensures length(xs) == length(tail(xs)) + 1
{
	match(xs)
		case Cons(y, ys) => ys
}

// List proofs
ghost method appendAssoc(l1:List, l2:List, l3:List)
ensures append(append(l1, l2), l3) == append(l1, append(l2, l3))
{

}

ghost method appendNil<T>(xs:List<T>)
ensures append(xs, Nil) == xs
{

			assert(run(append(compile(e), p), stack) == run( append( append( append( compile(e1), compile(e2) ), Cons(Add, Nil) ), p), stack));
			appendAssoc(append(compile(e1), compile(e2)), Cons(Add, Nil), p);
			appendAssoc(compile(e1), compile(e2), append(Cons(Add, Nil), p));
			assert(run( append( append( append( compile(e1), compile(e2) ), Cons(Add, Nil) ), p), stack) == run( append( compile(e1), append( compile(e2), append( Cons(Add, Nil), p))), stack));
			compileOK'(e1, append(append(compile(e2), Cons(Add, Nil)), p), stack);
			assert( run( append( compile(e1), append( compile(e2), append( Cons(Add, Nil), p))), stack) == run( append (compile(e2), append( Cons(Add, Nil), p)), Cons(eval(e1), stack)));
			compileOK'(e2, append(Cons(Add, Nil), p), Cons(eval(e1), stack));
			assert(run( append (compile(e2), append( Cons(Add, Nil), p)), Cons(eval(e1), stack))) == run( append( Cons(Add, Nil), p), Cons(eval(e2), Cons(eval(e1), stack)) );
			assert(run( append( Cons(Add, Nil), p), Cons(eval(e2), Cons(eval(e1), stack)) ) == run( p, Cons(eval(e1) + eval(e2), stack) ));
}
