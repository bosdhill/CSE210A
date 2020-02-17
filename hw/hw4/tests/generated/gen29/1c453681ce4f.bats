load ../../harness

@test "1c453681ce4f" {
  check 'z    :=    -4  +  0    ; 

  y :=-3   *  -2' '⇒ skip; y := (-3*-2), {z → -4}
⇒ y := (-3*-2), {z → -4}
⇒ skip, {y → 6, z → -4}'
}
