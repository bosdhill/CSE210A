load ../../harness

@test "ae932f37adc5" {
  check 'if (¬(z-     x     =   3))   then 

x := -1     *z  else y :=-2 +   1     ' '⇒ x := (-1*z), {}
⇒ skip, {x → 0}'
}
