load ../../harness

@test "490d1ad44e80" {
  check 'y :=y    -z;  y :=   -4   +     1;

skip   ' '⇒ skip; y := (-4+1); skip, {y → 0}
⇒ y := (-4+1); skip, {y → 0}
⇒ skip; skip, {y → -3}
⇒ skip, {y → -3}'
}
