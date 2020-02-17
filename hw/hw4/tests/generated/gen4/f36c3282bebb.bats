load ../../harness

@test "f36c3282bebb" {
  check 'if (z   -    -2 =     -2   +   -3∧false)     then x :=  4 +x    else 
 
z  :=z     +  4   ' '⇒ z := (z+4), {}
⇒ skip, {z → 4}'
}
