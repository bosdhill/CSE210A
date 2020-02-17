load ../../harness

@test "6b918771ea82" {
  check 'b    :=   1     *    -1 ;
y :=    -1  - -2    ' '⇒ skip; y := (-1--2), {b → -1}
⇒ y := (-1--2), {b → -1}
⇒ skip, {b → -1, y → 1}'
}
