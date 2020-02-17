load ../../harness

@test "f628c06a7c96" {
  check 'if (¬(y  *   z  =    r9    -y)) then z :=     0* -2     else RS  :=     x   ' '⇒ RS := x, {}
⇒ skip, {RS → 0}'
}
