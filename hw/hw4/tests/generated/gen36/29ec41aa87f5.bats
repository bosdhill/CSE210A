load ../../harness

@test "29ec41aa87f5" {
  check 'if (¬(3   *   x <x+    -2))     then x :=   md   -  2  else  
  skip' '⇒ x := (md-2), {}
⇒ skip, {x → -2}'
}
