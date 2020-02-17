load ../../harness

@test "578ef804d588" {
  check 'if (¬(-2 - 3 =     1+    -1))      then  
   x :=z *     k   else 
z :=x     +-4 ' '⇒ x := (z*k), {}
⇒ skip, {x → 0}'
}
