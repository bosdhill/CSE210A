load ../../harness

@test "4ef53986b910" {
  check 'if (true     ∧   4+  x <   -3    +     y)   then 
z  := -2     +     x else 
  y   :=     D2   - 1    ' '⇒ y := (D2-1), {}
⇒ skip, {y → -1}'
}
