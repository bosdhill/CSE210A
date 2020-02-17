load ../../harness

@test "0309032435f6" {
  check 'z := 4     *   -3  ;
  x   := y* x    ' '⇒ skip; x := (y*x), {z → -12}
⇒ x := (y*x), {z → -12}
⇒ skip, {x → 0, z → -12}'
}
