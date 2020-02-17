load ../../harness

@test "d5880325420a" {
  check 'OX    :=   x    *  x  ;


  y  :=  -2     *  -4' '⇒ skip; y := (-2*-4), {OX → 0}
⇒ y := (-2*-4), {OX → 0}
⇒ skip, {OX → 0, y → 8}'
}
