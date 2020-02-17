load ../../harness

@test "8b73860be2fd" {
  check 'z :=  x    -   y  ;
z   :=     v +-1   ' '⇒ skip; z := (v+-1), {z → 0}
⇒ z := (v+-1), {z → 0}
⇒ skip, {z → -1}'
}
