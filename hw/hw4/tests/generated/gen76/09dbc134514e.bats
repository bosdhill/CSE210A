load ../../harness

@test "09dbc134514e" {
  check 'while ¬true   ∧    y     -     x<   4+   z   do  
x    :=     1 -   z  ' '⇒ skip, {}'
}
