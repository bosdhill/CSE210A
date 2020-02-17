load ../../harness

@test "4239499b7cbe" {
  check 'z    :=   -1     - x    ;b     :=   1  *   -4     ' '⇒ skip; b := (1*-4), {z → -1}
⇒ b := (1*-4), {z → -1}
⇒ skip, {b → -4, z → -1}'
}
