load ../../harness

@test "fbfebee67198" {
  check 'while true ∧    false      do skip   ' '⇒ skip, {}'
}
