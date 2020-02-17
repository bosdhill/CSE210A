load ../../harness

@test "0bf52d208f0e" {
  check 'y   :=    -4     -  Q7   ;   qH     :=z   *  z  ' '⇒ skip; qH := (z*z), {y → -4}
⇒ qH := (z*z), {y → -4}
⇒ skip, {qH → 0, y → -4}'
}
