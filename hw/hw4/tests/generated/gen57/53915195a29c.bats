load ../../harness

@test "53915195a29c" {
  check 'if (-1    - y  <0    -   y    ∧true)   then K :=    z*T   else skip     ' '⇒ K := (z*T), {}
⇒ skip, {K → 0}'
}
