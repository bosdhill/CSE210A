load ../../harness

@test "79ad2145d8e5" {
  check 'y  :=   x   +     -4 ; z     :=     -1 * z    ' '⇒ skip; z := (-1*z), {y → -4}
⇒ z := (-1*z), {y → -4}
⇒ skip, {y → -4, z → 0}'
}
