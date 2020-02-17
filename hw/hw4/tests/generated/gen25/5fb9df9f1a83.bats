load ../../harness

@test "5fb9df9f1a83" {
  check 'if (4  *   2    =    y  *    z  ∨y    *     3   =s9     + y) then    y    :=y  + -2   else y:=    y  --4 ' '⇒ y := (y+-2), {}
⇒ skip, {y → -2}'
}
