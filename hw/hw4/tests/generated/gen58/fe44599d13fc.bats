load ../../harness

@test "fe44599d13fc" {
  check 'if (¬true)   then skip     else   
 x   :=   4  +    4    ' '⇒ x := (4+4), {}
⇒ skip, {x → 8}'
}
