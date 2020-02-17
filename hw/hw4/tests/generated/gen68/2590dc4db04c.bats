load ../../harness

@test "2590dc4db04c" {
  check 'if (¬true) then skip      else 
y   := y+  -3    ' '⇒ y := (y+-3), {}
⇒ skip, {y → -3}'
}
