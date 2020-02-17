load ../../harness

@test "b35275ede940" {
  check 'if (¬false)  then  z     := 2  *   4 else x     :=    -4 +   z   ' '⇒ z := (2*4), {}
⇒ skip, {z → 8}'
}
