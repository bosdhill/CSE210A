load ../../harness

@test "8c32c31df7ca" {
  check 'skip   ;x     :=    A  ' '⇒ x := A, {}
⇒ skip, {x → 0}'
}
