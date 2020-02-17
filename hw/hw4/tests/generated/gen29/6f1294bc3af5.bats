load ../../harness

@test "6f1294bc3af5" {
  check 'if (-4 *  -3   <     x   *    x)      then  
skip   else  y    := x   *   4     ' '⇒ y := (x*4), {}
⇒ skip, {y → 0}'
}
