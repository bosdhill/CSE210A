load ../../harness

@test "c46137753045" {
  check 'skip;z  :=    1    +  z     ' '⇒ z := (1+z), {}
⇒ skip, {z → 1}'
}
