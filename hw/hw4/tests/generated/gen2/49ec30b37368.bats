load ../../harness

@test "49ec30b37368" {
  check 'P8    := y  *  4  ;

y  :=     x*     L   ' '⇒ skip; y := (x*L), {P8 → 0}
⇒ y := (x*L), {P8 → 0}
⇒ skip, {P8 → 0, y → 0}'
}
