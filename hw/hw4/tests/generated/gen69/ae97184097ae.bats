load ../../harness

@test "ae97184097ae" {
  check 'while x   =y     +     1  ∨   -2   -    y+    2=     3   + 4   do x:=   0     -  3 ' '⇒ skip, {}'
}
