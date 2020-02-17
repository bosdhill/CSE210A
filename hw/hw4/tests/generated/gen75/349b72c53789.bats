load ../../harness

@test "349b72c53789" {
  check 'x   :=   z    +     x   ' '⇒ skip, {x → 0}'
}
