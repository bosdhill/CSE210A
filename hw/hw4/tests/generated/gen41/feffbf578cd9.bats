load ../../harness

@test "feffbf578cd9" {
  check 'z     :=     y  -   z    ;y:= y   - z  ' '⇒ skip; y := (y-z), {z → 0}
⇒ y := (y-z), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
