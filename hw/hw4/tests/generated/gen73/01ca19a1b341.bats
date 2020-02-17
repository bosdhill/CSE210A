load ../../harness

@test "01ca19a1b341" {
  check 'while x     + y   <   -1*     z∨1  -     x     <  z+    -3     do y:= 2     ' '⇒ skip, {}'
}
