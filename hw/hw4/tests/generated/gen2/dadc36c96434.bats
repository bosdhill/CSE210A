load ../../harness

@test "dadc36c96434" {
  check 'x    :=  -2   *     3   ;   
z:=z     ' '⇒ skip; z := z, {x → -6}
⇒ z := z, {x → -6}
⇒ skip, {x → -6, z → 0}'
}
