load ../../harness

@test "74e7a4a333a4" {
  check 'y    := 4     + x   ;  

 y:=    z *     -2   ' '⇒ skip; y := (z*-2), {y → 4}
⇒ y := (z*-2), {y → 4}
⇒ skip, {y → 0}'
}
