load ../../harness

@test "564ef8647c38" {
  check 'if (mv   *  2   <     -4∨ false)  then 
y     :=    -4     *  y  else 
 y   :=  x    + 2  ' '⇒ y := (x+2), {}
⇒ skip, {y → 2}'
}
