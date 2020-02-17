load ../../harness

@test "7bcd8930365b" {
  check 'if (¬true) then skip else  y  :=  x' '⇒ y := x, {}
⇒ skip, {y → 0}'
}
