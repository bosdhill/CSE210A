load ../../harness

@test "ca8afe31b33c" {
  check 'y  :=   z   *    y ;

y:=     -3     -  x  ' '⇒ skip; y := (-3-x), {y → 0}
⇒ y := (-3-x), {y → 0}
⇒ skip, {y → -3}'
}
