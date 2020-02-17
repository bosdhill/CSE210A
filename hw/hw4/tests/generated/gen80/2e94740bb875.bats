load ../../harness

@test "2e94740bb875" {
  check 'if (¬(y + x     = 1    - -2))      then z := y    *V   else x:=  O     +     z ' '⇒ z := (y*V), {}
⇒ skip, {z → 0}'
}
