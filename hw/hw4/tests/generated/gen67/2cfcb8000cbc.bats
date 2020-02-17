load ../../harness

@test "2cfcb8000cbc" {
  check 'skip     ; z    :=  z     +  -2    ' '⇒ z := (z+-2), {}
⇒ skip, {z → -2}'
}
