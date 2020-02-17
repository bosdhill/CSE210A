load ../../harness

@test "aa08998e20d4" {
  check 'y  :=   -1     *   1   ; 
Z:=     tI   * z  ' '⇒ skip; Z := (tI*z), {y → -1}
⇒ Z := (tI*z), {y → -1}
⇒ skip, {Z → 0, y → -1}'
}
