load ../../harness

@test "67a09fe47596" {
  check 'y :=    x+0    ;

skip     ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
