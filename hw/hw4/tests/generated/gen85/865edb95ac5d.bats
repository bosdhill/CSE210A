load ../../harness

@test "865edb95ac5d" {
  check 'if (3   - -4   < E3  -   y    ∧  z   =    4     +   -1)     then   
bq  :=     4 - Q    else y    :=   y     *   -1  ' '⇒ y := (y*-1), {}
⇒ skip, {y → 0}'
}
