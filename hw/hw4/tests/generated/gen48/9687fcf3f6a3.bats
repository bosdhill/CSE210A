load ../../harness

@test "9687fcf3f6a3" {
  check 'y    := (M0  +    he)     *     z    ;
 z     :=  -3     - 4  ' '⇒ skip; z := (-3-4), {y → 0}
⇒ z := (-3-4), {y → 0}
⇒ skip, {y → 0, z → -7}'
}
