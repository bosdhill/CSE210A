load ../../harness

@test "78e6f0454f53" {
  check 'if (false     ∧  y *     y   <  -3   + C0)     then  x  :=-1  +    z else 
 x  := -3   *     y    ' '⇒ x := (-3*y), {}
⇒ skip, {x → 0}'
}
