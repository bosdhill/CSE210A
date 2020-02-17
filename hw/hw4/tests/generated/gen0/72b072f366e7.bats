load ../../harness

@test "72b072f366e7" {
  check 'if (4    +1  <   U     - 0 ∨   false)    then y  :=2+    y   else 
skip     ' '⇒ skip, {}'
}
