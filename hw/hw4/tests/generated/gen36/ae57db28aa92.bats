load ../../harness

@test "ae57db28aa92" {
  check 'skip   ;    z     :=    y     +y     ' '⇒ z := (y+y), {}
⇒ skip, {z → 0}'
}
