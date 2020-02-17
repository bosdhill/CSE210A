load ../../harness

@test "86b164156978" {
  check 'if (true    ∨ false) then 
y    :=     2     *     x  else    y     :=    -4 *z' '⇒ y := (2*x), {}
⇒ skip, {y → 0}'
}
