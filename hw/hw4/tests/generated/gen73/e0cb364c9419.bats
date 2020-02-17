load ../../harness

@test "e0cb364c9419" {
  check 'z     := 4- -2 ;
y    :=  3    +     -2' '⇒ skip; y := (3+-2), {z → 6}
⇒ y := (3+-2), {z → 6}
⇒ skip, {y → 1, z → 6}'
}
