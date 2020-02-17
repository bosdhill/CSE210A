load ../../harness

@test "ec0bc49fc80f" {
  check 'if (¬(-3 * x=     y *    y)) then    

x :=   y   *    D   else  
x    :=     y   *0' '⇒ x := (y*0), {}
⇒ skip, {x → 0}'
}
