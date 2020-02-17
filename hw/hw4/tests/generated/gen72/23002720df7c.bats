load ../../harness

@test "23002720df7c" {
  check 'while (¬(y  +     x   <1    -   y))   do  

 y    :=   x  + 3    ' '⇒ skip, {}'
}
