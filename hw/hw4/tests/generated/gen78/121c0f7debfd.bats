load ../../harness

@test "121c0f7debfd" {
  check 'x     :=x    -  q ' '⇒ skip, {x → 0}'
}
