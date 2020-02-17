load ../../harness

@test "65f893473573" {
  check 'qN :=     2    * x     ; T6  :=  z +    2  ' '⇒ skip; T6 := (z+2), {qN → 0}
⇒ T6 := (z+2), {qN → 0}
⇒ skip, {T6 → 2, qN → 0}'
}
