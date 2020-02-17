load ../../harness

@test "87af7d6f9940" {
  check 'y :=    x-    x ' '⇒ skip, {y → 0}'
}
