load ../../harness

@test "92d8e1ad7be5" {
  check 'if (false ∨     L <  0     -   0)  then 
x   :=2    -   2  else  
skip    ' '⇒ skip, {}'
}
