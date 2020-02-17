load ../../harness

@test "92555579f5f0" {
  check 'if (false    ∧    2+    y  =-1*   x)  then 
y :=   X2  -  x     else 
 x   :=    -1    ' '⇒ x := -1, {}
⇒ skip, {x → -1}'
}
