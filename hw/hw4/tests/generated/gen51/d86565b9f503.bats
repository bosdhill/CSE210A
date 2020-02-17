load ../../harness

@test "d86565b9f503" {
  check 'z :=   y     ;
y :=    4 -  z    ' '⇒ skip; y := (4-z), {z → 0}
⇒ y := (4-z), {z → 0}
⇒ skip, {y → 4, z → 0}'
}
