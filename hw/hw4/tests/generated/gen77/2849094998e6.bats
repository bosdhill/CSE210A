load ../../harness

@test "2849094998e6" {
  check 'if (true ∨ false) then 
p :=  w     +   3      else 
z   :=1     -   z    ' '⇒ p := (w+3), {}
⇒ skip, {p → 3}'
}
