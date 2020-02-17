load ../../harness

@test "608363c4635b" {
  check 'if (true     ∨  x     *x=   -1   *    1)  then   
x:=     -1 *   o    else  x:= Gf   +     2' '⇒ x := (-1*o), {}
⇒ skip, {x → 0}'
}
