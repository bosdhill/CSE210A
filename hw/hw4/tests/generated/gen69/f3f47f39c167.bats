load ../../harness

@test "f3f47f39c167" {
  check 'while true∧     false      do  a   :=    -4    -  z ' '⇒ skip, {}'
}
