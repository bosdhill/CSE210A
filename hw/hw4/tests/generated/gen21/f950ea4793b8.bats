load ../../harness

@test "f950ea4793b8" {
  check 'if (¬(0  *   z   =     0+    y))     then 



 skip    else 
y:=     -1 -z' '⇒ y := (-1-z), {}
⇒ skip, {y → -1}'
}
