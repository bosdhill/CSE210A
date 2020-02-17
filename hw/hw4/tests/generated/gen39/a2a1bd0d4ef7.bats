load ../../harness

@test "a2a1bd0d4ef7" {
  check 'y    :=  -3  +     U    ;

 y   :=     y*    3  ' '⇒ skip; y := (y*3), {y → -3}
⇒ y := (y*3), {y → -3}
⇒ skip, {y → -9}'
}
