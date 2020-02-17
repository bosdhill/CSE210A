load ../../harness

@test "f7205252cfeb" {
  check 'z    :=   y  - 3 ;

  x:=    y   - 2  ' '⇒ skip; x := (y-2), {z → -3}
⇒ x := (y-2), {z → -3}
⇒ skip, {x → -2, z → -3}'
}
