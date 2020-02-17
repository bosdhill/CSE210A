load ../../harness

@test "2f278d201a55" {
  check 'if (true   ∧  4 -  x   =4     -   2)     then 
x  :=  2-   z     else  skip    ' '⇒ skip, {}'
}
