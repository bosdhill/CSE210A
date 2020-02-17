load ../../harness

@test "5ed1635ea6d7" {
  check 'if (¬(x+    -4 =1))     then  skip     else  x    :=     0  * z    ' '⇒ skip, {}'
}
