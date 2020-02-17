load ../../harness

@test "5a4a21194345" {
  check 'if (¬false)    then y   :=  -2   *x  else 

z    :=  z +   y' '⇒ y := (-2*x), {}
⇒ skip, {y → 0}'
}
