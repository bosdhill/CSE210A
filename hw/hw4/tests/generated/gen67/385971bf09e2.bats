load ../../harness

@test "385971bf09e2" {
  check 'z:=    -1  +   x   ;z  :=  4 +   1    ' '⇒ skip; z := (4+1), {z → -1}
⇒ z := (4+1), {z → -1}
⇒ skip, {z → 5}'
}
