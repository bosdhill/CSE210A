load ../../harness

@test "b009bda73496" {
  check 'if false     then skip     else 
y := x * 2   ' '⇒ y := (x*2), {}
⇒ skip, {y → 0}'
}
