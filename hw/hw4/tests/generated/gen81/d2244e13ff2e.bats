load ../../harness

@test "d2244e13ff2e" {
  check 'y:=  3   *z ;y:=x   -z ' '⇒ skip; y := (x-z), {y → 0}
⇒ y := (x-z), {y → 0}
⇒ skip, {y → 0}'
}
