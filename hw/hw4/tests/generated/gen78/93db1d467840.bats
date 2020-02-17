load ../../harness

@test "93db1d467840" {
  check 'y  := 4    *    -1 ;
skip' '⇒ skip; skip, {y → -4}
⇒ skip, {y → -4}'
}
