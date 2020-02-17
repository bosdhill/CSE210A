load ../../harness

@test "a27b4b2db792" {
  check 'z   :=    -3     -     3    ; z  :=     x     + z     ' '⇒ skip; z := (x+z), {z → -6}
⇒ z := (x+z), {z → -6}
⇒ skip, {z → -6}'
}
