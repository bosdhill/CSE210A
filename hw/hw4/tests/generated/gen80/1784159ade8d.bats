load ../../harness

@test "1784159ade8d" {
  check 'if (x  + -4   =  -4-    1     ∨   3     +    -4  =    z  + z) then   skip   else 
skip   ' '⇒ skip, {}'
}
