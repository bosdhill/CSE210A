load ../../harness

@test "b00719fed594" {
  check 'y := 1*z    ;y    :=    4    +  P    ' '⇒ skip; y := (4+P), {y → 0}
⇒ y := (4+P), {y → 0}
⇒ skip, {y → 4}'
}
