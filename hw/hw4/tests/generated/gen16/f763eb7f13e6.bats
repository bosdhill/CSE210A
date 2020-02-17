load ../../harness

@test "f763eb7f13e6" {
  check 'z    :=    4    *y  *    2   ; 
B  :=  -2     ' '⇒ skip; B := -2, {z → 0}
⇒ B := -2, {z → 0}
⇒ skip, {B → -2, z → 0}'
}
