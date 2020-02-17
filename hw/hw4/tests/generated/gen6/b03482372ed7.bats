load ../../harness

@test "b03482372ed7" {
  check 'while (¬(x  - 2< -1     +4)) do z:= -4 +1   ' '⇒ skip, {}'
}
