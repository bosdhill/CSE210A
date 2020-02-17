load ../../harness

@test "60e10fdc9130" {
  check 'if (true  ∨     false)     then 
qx :=     -2-    -1     else   x:=x   + 4    ' '⇒ qx := (-2--1), {}
⇒ skip, {qx → -1}'
}
