load ../../harness

@test "03087cc9b772" {
  check 'R4  := -4  +   x ;
 D5     := -2     *    y     ' '⇒ skip; D5 := (-2*y), {R4 → -4}
⇒ D5 := (-2*y), {R4 → -4}
⇒ skip, {D5 → 0, R4 → -4}'
}
