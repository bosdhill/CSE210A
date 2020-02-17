load ../../harness

@test "566b46140864" {
  check 'y     :=  -3  ;z    :=d  * y    ' '⇒ skip; z := (d*y), {y → -3}
⇒ z := (d*y), {y → -3}
⇒ skip, {y → -3, z → 0}'
}
