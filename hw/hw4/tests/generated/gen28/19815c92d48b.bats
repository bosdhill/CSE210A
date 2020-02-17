load ../../harness

@test "19815c92d48b" {
  check 'if (false∧true)  then 
y     := -3-    y  else 
   rZ     :=   x + -3' '⇒ rZ := (x+-3), {}
⇒ skip, {rZ → -3}'
}
