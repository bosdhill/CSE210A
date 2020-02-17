load ../../harness

@test "bcc6bea4a67e" {
  check 'y  :=-4  -x     ; 
 y   := x+    1   ' '⇒ skip; y := (x+1), {y → -4}
⇒ y := (x+1), {y → -4}
⇒ skip, {y → 1}'
}
