load ../../harness

@test "0838aa0e65f9" {
  check 'z:=  2  *     2 ;
  
z :=    -2+    0 ' '⇒ skip; z := (-2+0), {z → 4}
⇒ z := (-2+0), {z → 4}
⇒ skip, {z → -2}'
}
