load ../../harness

@test "592520f747e7" {
  check 'z := 1 + y; 

 y     :=    y     -    y' '⇒ skip; y := (y-y), {z → 1}
⇒ y := (y-y), {z → 1}
⇒ skip, {y → 0, z → 1}'
}
