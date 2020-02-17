load ../../harness

@test "dbef89b3355d" {
  check 'if ((x    +     y =   3 ∨ y    -   x=0 -    0)∧  true)    then  z     := y  - y     else   skip ' '⇒ z := (y-y), {}
⇒ skip, {z → 0}'
}
