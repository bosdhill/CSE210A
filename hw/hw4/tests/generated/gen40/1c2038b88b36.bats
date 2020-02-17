load ../../harness

@test "1c2038b88b36" {
  check 'if (true   ∧   2+  -2  < 3   - x) then 
 z  :=   -4 *   -3   else  
 
y  :=    x   -   2' '⇒ z := (-4*-3), {}
⇒ skip, {z → 12}'
}
