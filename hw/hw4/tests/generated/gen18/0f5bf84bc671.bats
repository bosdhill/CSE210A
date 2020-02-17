load ../../harness

@test "0f5bf84bc671" {
  check 'while true∧     false      do    c3:= M    - -3     ' '⇒ skip, {}'
}
