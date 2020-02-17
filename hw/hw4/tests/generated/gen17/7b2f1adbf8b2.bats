load ../../harness

@test "7b2f1adbf8b2" {
  check 'y     :=     x*     -1 ;



x    := -4     +    y' '⇒ skip; x := (-4+y), {y → 0}
⇒ x := (-4+y), {y → 0}
⇒ skip, {x → -4, y → 0}'
}
