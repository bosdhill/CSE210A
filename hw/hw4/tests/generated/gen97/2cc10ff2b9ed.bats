load ../../harness

@test "2cc10ff2b9ed" {
  check 'skip  ;y:=l *     x   ' '⇒ y := (l*x), {}
⇒ skip, {y → 0}'
}
