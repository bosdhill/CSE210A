load ../../harness

@test "26c9aa7c6d91" {
  check 'if (z    = 2    +-1     ∧  x+ 0= y     * x) then  skip     else 

   x   :=   x  -    4  ' '⇒ x := (x-4), {}
⇒ skip, {x → -4}'
}
