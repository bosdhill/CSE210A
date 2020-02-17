load ../../harness

@test "dd573c6f2afd" {
  check 'skip;yV :=    y    * -1  ' '⇒ yV := (y*-1), {}
⇒ skip, {yV → 0}'
}
