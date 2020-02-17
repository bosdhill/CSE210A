load ../../harness

@test "f6e373e3360a" {
  check 'y    :=  1     +    1     ;
y    :=     -3  * -4  ' '⇒ skip; y := (-3*-4), {y → 2}
⇒ y := (-3*-4), {y → 2}
⇒ skip, {y → 12}'
}
