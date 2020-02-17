load ../../harness

@test "75ab91cb0e16" {
  check 'if (true∨     Z6     <  -3+    2)   then 
skip      else skip' '⇒ skip, {}'
}
