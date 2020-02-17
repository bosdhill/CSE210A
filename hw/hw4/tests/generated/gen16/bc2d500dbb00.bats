load ../../harness

@test "bc2d500dbb00" {
  check 'skip    ;   bq  :=  y    +-3' '⇒ bq := (y+-3), {}
⇒ skip, {bq → -3}'
}
