load ../../harness

@test "a5ec066f76c4" {
  check 'y  := 3    *     -4     ;
skip  ' '⇒ skip; skip, {y → -12}
⇒ skip, {y → -12}'
}
