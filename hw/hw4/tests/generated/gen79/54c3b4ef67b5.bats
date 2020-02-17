load ../../harness

@test "54c3b4ef67b5" {
  check 'z    :=    z  + -2     ;x   :=     -1  *   z ' '⇒ skip; x := (-1*z), {z → -2}
⇒ x := (-1*z), {z → -2}
⇒ skip, {x → 2, z → -2}'
}
