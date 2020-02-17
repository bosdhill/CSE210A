load ../../harness

@test "1aac54e61909" {
  check 'z := z-   4  ;  z   :=    1     *y     ' '⇒ skip; z := (1*y), {z → -4}
⇒ z := (1*y), {z → -4}
⇒ skip, {z → 0}'
}
