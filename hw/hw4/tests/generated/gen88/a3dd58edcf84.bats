load ../../harness

@test "a3dd58edcf84" {
  check 'skip     ; x     := x   *   2  ' '⇒ x := (x*2), {}
⇒ skip, {x → 0}'
}
