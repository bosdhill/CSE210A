load ../../harness

@test "bff58c6ed0a4" {
  check 'if (-3- x<  y  *     -3  ∨     y* x  <    -2   -    1)   then  
skip else 
y :=   x + 0    ' '⇒ skip, {}'
}
