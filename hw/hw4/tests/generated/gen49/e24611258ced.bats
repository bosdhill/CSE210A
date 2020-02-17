load ../../harness

@test "e24611258ced" {
  check 'if (¬true)     then   skip   else  
  z :=  -1    -  -1     ' '⇒ z := (-1--1), {}
⇒ skip, {z → 0}'
}
