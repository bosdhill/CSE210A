load ../../harness

@test "2a6441bb3640" {
  check 'x     :=    LE  *  -1     ' '⇒ skip, {x → 0}'
}
