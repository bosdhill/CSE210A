load ../../harness

@test "f0cb4d584dfd" {
  check 'if (true    ∨ x  +  y  < z)   then  
x   :=  z     +  z  else  
x:=2   *   y  ' '⇒ x := (z+z), {}
⇒ skip, {x → 0}'
}
