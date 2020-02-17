load ../../harness

@test "2f72057bb2a4" {
  check 'if (x    *     y     < -2 + -1    ∧   true)     then    y  :=    1    +     f     else  
EO  :=     0-  -1' '⇒ EO := (0--1), {}
⇒ skip, {EO → 1}'
}
