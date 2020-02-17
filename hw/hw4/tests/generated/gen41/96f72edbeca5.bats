load ../../harness

@test "96f72edbeca5" {
  check 'x   :=    1    -     x  ' '⇒ skip, {x → 1}'
}
