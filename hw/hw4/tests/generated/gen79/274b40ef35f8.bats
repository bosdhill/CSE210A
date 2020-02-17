load ../../harness

@test "274b40ef35f8" {
  check 'if (-4     *     x=   -1     -     z   ∧ true)     then  y :=  y    - -4     else    
z :=   x*     (2+     -4)  ' '⇒ z := (x*(2+-4)), {}
⇒ skip, {z → 0}'
}
