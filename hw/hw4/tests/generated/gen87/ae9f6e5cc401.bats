load ../../harness

@test "ae9f6e5cc401" {
  check 'skip    ;x   :=   x *   z     ' '⇒ x := (x*z), {}
⇒ skip, {x → 0}'
}
