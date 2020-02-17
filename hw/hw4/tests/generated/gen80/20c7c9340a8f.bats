load ../../harness

@test "20c7c9340a8f" {
  check 'z     := 1  *   y;
    x     :=     -2 * z  ' '⇒ skip; x := (-2*z), {z → 0}
⇒ x := (-2*z), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
