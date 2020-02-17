load ../../harness

@test "d35d4f31e8ac" {
  check 'z :=   x+  -2     ;

 
 y     :=z -   0' '⇒ skip; y := (z-0), {z → -2}
⇒ y := (z-0), {z → -2}
⇒ skip, {y → -2, z → -2}'
}
