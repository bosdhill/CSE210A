load ../../harness

@test "96677abde377" {
  check 'skip; 
y    := x+    -1   ' '⇒ y := (x+-1), {}
⇒ skip, {y → -1}'
}
