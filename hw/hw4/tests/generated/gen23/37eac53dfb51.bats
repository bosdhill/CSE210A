load ../../harness

@test "37eac53dfb51" {
  check 'if (false∧  y  -    x    <   -4)     then  x :=    0  * N   else y     :=2  -     -2     ' '⇒ y := (2--2), {}
⇒ skip, {y → 4}'
}
