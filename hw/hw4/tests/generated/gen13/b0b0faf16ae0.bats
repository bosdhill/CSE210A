load ../../harness

@test "b0b0faf16ae0" {
  check 'if (m     * 2     <    z     +     4    ∧    -1    + z    =     4   -  x) then   y    := -2   +  Z1 else 
y   :=   x  -   y     ' '⇒ y := (x-y), {}
⇒ skip, {y → 0}'
}
