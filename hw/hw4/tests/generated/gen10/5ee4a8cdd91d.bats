load ../../harness

@test "5ee4a8cdd91d" {
  check 'vM  :=   4    *    1 ;z  :=    0   +    1 ' '⇒ skip; z := (0+1), {vM → 4}
⇒ z := (0+1), {vM → 4}
⇒ skip, {vM → 4, z → 1}'
}
