load ../../harness

@test "fe3263a654b8" {
  check 'while 0   -   2   =2 -     x   ∧ true    do    skip     ' '⇒ skip, {}'
}
