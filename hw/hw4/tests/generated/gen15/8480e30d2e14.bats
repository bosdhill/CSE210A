load ../../harness

@test "8480e30d2e14" {
  check 'if (¬true)    then x:=   -1*   y  else  
 
x    :=     -3   -   -3' '⇒ x := (-3--3), {}
⇒ skip, {x → 0}'
}
