load ../../harness

@test "cf78af2f1de2" {
  check 'z    :=    0  -1   ;
y     :=     z -     2  ' '⇒ skip; y := (z-2), {z → -1}
⇒ y := (z-2), {z → -1}
⇒ skip, {y → -3, z → -1}'
}
