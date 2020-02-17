load ../../harness

@test "4b924a16d090" {
  check 'if (¬false)      then 
  skip     else   y   :=   b8+    y' '⇒ skip, {}'
}
