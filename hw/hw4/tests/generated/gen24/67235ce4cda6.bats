load ../../harness

@test "67235ce4cda6" {
  check 'y:=     4  *     y ;
skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
