load ../../harness

@test "a61e64651306" {
  check 'if (3+    -1<    2     - 3 ∨ false)  then 

skip  else 
skip    ' '⇒ skip, {}'
}
