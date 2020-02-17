load ../../harness

@test "b208ab404e64" {
  check 'skip;y   :=R     -x     ' '⇒ y := (R-x), {}
⇒ skip, {y → 0}'
}
