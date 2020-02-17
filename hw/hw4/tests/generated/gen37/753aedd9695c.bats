load ../../harness

@test "753aedd9695c" {
  check 'if (false     ∨     true)      then 
   skip     else    x     :=     2  -    x     ' '⇒ skip, {}'
}
