load ../../harness

@test "850bb3871fab" {
  check 'if (1   *   y    <  2 -     y  ∧  3+     nl=    0*   -3)     then skip     else 
   skip' '⇒ skip, {}'
}
