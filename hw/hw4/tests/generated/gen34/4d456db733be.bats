load ../../harness

@test "4d456db733be" {
  check 'x    :=     0   -    2     ; 

y   :=-3 *     -4' '⇒ skip; y := (-3*-4), {x → -2}
⇒ y := (-3*-4), {x → -2}
⇒ skip, {x → -2, y → 12}'
}
