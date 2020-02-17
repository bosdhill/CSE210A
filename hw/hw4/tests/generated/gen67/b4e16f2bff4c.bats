load ../../harness

@test "b4e16f2bff4c" {
  check 'if (true ∧  false) then  
 
y    :=   z -x else x  :=  x  -1 ' '⇒ x := (x-1), {}
⇒ skip, {x → -1}'
}
