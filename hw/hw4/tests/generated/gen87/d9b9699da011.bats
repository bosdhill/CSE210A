load ../../harness

@test "d9b9699da011" {
  check 'if (4    +   1    <  2-   -2     ∧  -2   -     x  = z     +     3)    then 

y  :=    z   * z  else 

y     :=   z     -   y    ' '⇒ y := (z-y), {}
⇒ skip, {y → 0}'
}
