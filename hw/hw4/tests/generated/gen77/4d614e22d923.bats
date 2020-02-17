load ../../harness

@test "4d614e22d923" {
  check 'if (x *y=     z  + -4   ∨  x    *   x     <    t+  -3)   then    
skip   else y  := 0     ' '⇒ y := 0, {}
⇒ skip, {y → 0}'
}
