load ../../harness

@test "3631c3a0d6e6" {
  check 'skip     ; 
   x    := 1     +    y ' '⇒ x := (1+y), {}
⇒ skip, {x → 1}'
}
