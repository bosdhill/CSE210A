load ../../harness

@test "f9fd7899a8fb" {
  check 'if (¬(2    +     y  =     2   *  -2))   then 
x     :=   0  *     x  else skip   ' '⇒ x := (0*x), {}
⇒ skip, {x → 0}'
}
