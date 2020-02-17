load ../../harness

@test "e3e7f708866d" {
  check 'if (z   *   z = x     -x     ∧  true)      then 
   skip  else    
z   :=     y   -  -3' '⇒ skip, {}'
}
