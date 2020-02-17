load ../../harness

@test "d1615bb4e7a3" {
  check 'x:=  0 ;  z    := y     --2     ' '⇒ skip; z := (y--2), {x → 0}
⇒ z := (y--2), {x → 0}
⇒ skip, {x → 0, z → 2}'
}
