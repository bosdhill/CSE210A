load ../../harness

@test "43fcd0385cda" {
  check 'x   :=y   *   L     ;  


y    :=     -3*     -4' '⇒ skip; y := (-3*-4), {x → 0}
⇒ y := (-3*-4), {x → 0}
⇒ skip, {x → 0, y → 12}'
}
