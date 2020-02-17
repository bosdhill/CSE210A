load ../../harness

@test "b70167584c1d" {
  check 'y :=  z*  -2;
 y :=    -4    -     z  ' '⇒ skip; y := (-4-z), {y → 0}
⇒ y := (-4-z), {y → 0}
⇒ skip, {y → -4}'
}
