load ../../harness

@test "c1b2dff1d8f5" {
  check 'z    :=0   + y     ; z     :=   y  +q    ' '⇒ skip; z := (y+q), {z → 0}
⇒ z := (y+q), {z → 0}
⇒ skip, {z → 0}'
}
