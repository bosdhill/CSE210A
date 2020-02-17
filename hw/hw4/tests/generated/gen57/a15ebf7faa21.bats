load ../../harness

@test "a15ebf7faa21" {
  check 'if (2+  x   <   z   -x     ∧x*   1 <     z*2)     then  y:=     z    -     z   else y :=  -3-    2  ' '⇒ y := (-3-2), {}
⇒ skip, {y → -5}'
}
