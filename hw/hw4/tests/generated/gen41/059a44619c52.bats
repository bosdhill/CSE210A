load ../../harness

@test "059a44619c52" {
  check 'skip  ;  y   :=  y  -  0     ' '⇒ y := (y-0), {}
⇒ skip, {y → 0}'
}
