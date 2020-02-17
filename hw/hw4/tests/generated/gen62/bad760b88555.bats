load ../../harness

@test "bad760b88555" {
  check 'skip     ;
y := z  *   (y  + -2)    ' '⇒ y := (z*(y+-2)), {}
⇒ skip, {y → 0}'
}
