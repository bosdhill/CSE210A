load ../../harness

@test "99a486c4ab0f" {
  check 'y :=  2   +     -1 ;  z    :=    x +  2 ' '⇒ skip; z := (x+2), {y → 1}
⇒ z := (x+2), {y → 1}
⇒ skip, {y → 1, z → 2}'
}
