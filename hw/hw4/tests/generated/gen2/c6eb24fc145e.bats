load ../../harness

@test "c6eb24fc145e" {
  check 'skip    ;x     :=    -4     +     -1 ' '⇒ x := (-4+-1), {}
⇒ skip, {x → -5}'
}
