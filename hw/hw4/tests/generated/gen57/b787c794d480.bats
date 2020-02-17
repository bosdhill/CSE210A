load ../../harness

@test "b787c794d480" {
  check 'x :=    x   *    y   ;
 
b  :=     -4   -  -1' '⇒ skip; b := (-4--1), {x → 0}
⇒ b := (-4--1), {x → 0}
⇒ skip, {b → -3, x → 0}'
}
