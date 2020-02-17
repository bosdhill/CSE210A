load ../../harness

@test "b9d9333c3d48" {
  check 'z := 4 *-2 ;  


y    :=   y     -     2' '⇒ skip; y := (y-2), {z → -8}
⇒ y := (y-2), {z → -8}
⇒ skip, {y → -2, z → -8}'
}
