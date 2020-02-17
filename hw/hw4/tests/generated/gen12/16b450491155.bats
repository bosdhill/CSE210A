load ../../harness

@test "16b450491155" {
  check 'y :=x    ;y  :=-2 *  y ' '⇒ skip; y := (-2*y), {y → 0}
⇒ y := (-2*y), {y → 0}
⇒ skip, {y → 0}'
}
