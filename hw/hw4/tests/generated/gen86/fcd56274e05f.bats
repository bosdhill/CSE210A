load ../../harness

@test "fcd56274e05f" {
  check 'if (3  --3 <x    *  z   ∧   x    -z <  zm    - y)    then  
 
skip    else x   :=     y    *  z   ' '⇒ x := (y*z), {}
⇒ skip, {x → 0}'
}
