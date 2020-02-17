load ../../harness

@test "a146c9101b87" {
  check 'if (¬false)      then  x  :=   2  *     y      else   
 skip    ' '⇒ x := (2*y), {}
⇒ skip, {x → 0}'
}
