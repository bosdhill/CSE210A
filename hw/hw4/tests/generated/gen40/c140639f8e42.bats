load ../../harness

@test "c140639f8e42" {
  check 'if (z -     X0<    -2     +     0  ∧     false)  then 

skip   else 
x     :=   -4    *     -1     ' '⇒ x := (-4*-1), {}
⇒ skip, {x → 4}'
}
