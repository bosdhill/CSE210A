load ../../harness

@test "a2417a53429b" {
  check 'skip    ;  y :=  3     + x    ' '⇒ y := (3+x), {}
⇒ skip, {y → 3}'
}
