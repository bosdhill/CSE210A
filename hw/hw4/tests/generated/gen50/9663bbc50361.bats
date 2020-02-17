load ../../harness

@test "9663bbc50361" {
  check 'if (true  ∧z* 3  <  3+    x)   then 
skip      else skip     ' '⇒ skip, {}'
}
