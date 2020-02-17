load ../../harness

@test "9b40366e87b4" {
  check 'if (HD     + y  =  2  + 0     ∧   true)  then  x   :=     -1 +  z   else 
 
z:=     4   +   -4' '⇒ z := (4+-4), {}
⇒ skip, {z → 0}'
}
