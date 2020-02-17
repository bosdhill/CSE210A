load ../../harness

@test "3c94272af5c5" {
  check 'if (x     * z   =    -4 *   y∨   z  - y     =  -3     +  x)    then   

y   :=     -1 *z   else skip   ' '⇒ y := (-1*z), {}
⇒ skip, {y → 0}'
}
