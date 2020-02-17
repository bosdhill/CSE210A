load ../../harness

@test "1fe03721ae6b" {
  check 'if (¬(-4+    z     =     -4 -    -3))      then  
x   :=     -2  *     4   else 
skip' '⇒ x := (-2*4), {}
⇒ skip, {x → -8}'
}
