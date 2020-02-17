load ../../harness

@test "b8af6ded970c" {
  check 'if (0  -  x   < y  *   -2)   then 
skip   else    x:=  3  ' '⇒ x := 3, {}
⇒ skip, {x → 3}'
}
