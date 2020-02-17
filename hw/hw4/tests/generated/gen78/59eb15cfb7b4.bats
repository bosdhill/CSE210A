load ../../harness

@test "59eb15cfb7b4" {
  check 'if (-1 *x=    y    -  0∨true)     then 
  y    :=     0     * 2   else  
 z   :=     y     -     x     ' '⇒ y := (0*2), {}
⇒ skip, {y → 0}'
}
