load ../../harness

@test "ea0654447806" {
  check 'if (¬false)      then  z :=    -2   +   0  else  
Uh:=     -1-    2     +x     ' '⇒ z := (-2+0), {}
⇒ skip, {z → -2}'
}
