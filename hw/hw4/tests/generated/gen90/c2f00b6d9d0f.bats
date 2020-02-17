load ../../harness

@test "c2f00b6d9d0f" {
  check 'if (¬false)     then z     :=    y  +     e   else skip     ' '⇒ z := (y+e), {}
⇒ skip, {z → 0}'
}
