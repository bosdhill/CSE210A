load ../../harness

@test "eeb596366170" {
  check 'td     :=  2   *  v*   x   ; 
x  :=    1 ' '⇒ skip; x := 1, {td → 0}
⇒ x := 1, {td → 0}
⇒ skip, {td → 0, x → 1}'
}
