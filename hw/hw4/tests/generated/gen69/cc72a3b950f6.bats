load ../../harness

@test "cc72a3b950f6" {
  check 'if (true ∨    2-    -1  <  0)    then x:=   Yu*  z    else 
z:=     z    +  -2  ' '⇒ x := (Yu*z), {}
⇒ skip, {x → 0}'
}
