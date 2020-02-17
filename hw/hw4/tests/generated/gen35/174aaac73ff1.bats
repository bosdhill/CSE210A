load ../../harness

@test "174aaac73ff1" {
  check 'if (true∨  false)    then   

y     :=    -4 *    z   else  skip' '⇒ y := (-4*z), {}
⇒ skip, {y → 0}'
}
