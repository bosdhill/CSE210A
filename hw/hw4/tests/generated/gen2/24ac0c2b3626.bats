load ../../harness

@test "24ac0c2b3626" {
  check 'x    :=  2   +  -4 ' '⇒ skip, {x → -2}'
}
