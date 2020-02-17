load ../../harness

@test "bdc8bf24b090" {
  check 'z    :=     0  + y   ;
z    :=  3    ' '⇒ skip; z := 3, {z → 0}
⇒ z := 3, {z → 0}
⇒ skip, {z → 3}'
}
