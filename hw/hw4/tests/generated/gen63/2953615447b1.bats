load ../../harness

@test "2953615447b1" {
  check 'x   :=-4  -   -3 ' '⇒ skip, {x → -1}'
}
