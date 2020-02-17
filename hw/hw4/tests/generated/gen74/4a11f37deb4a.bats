load ../../harness

@test "4a11f37deb4a" {
  check 'x   :=    -1     - x    ' '⇒ skip, {x → -1}'
}
