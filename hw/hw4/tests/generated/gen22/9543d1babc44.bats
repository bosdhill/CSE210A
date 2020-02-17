load ../../harness

@test "9543d1babc44" {
  check 'while true    ∧    false  do skip  ' '⇒ skip, {}'
}
