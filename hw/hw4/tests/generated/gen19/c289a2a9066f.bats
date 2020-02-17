load ../../harness

@test "c289a2a9066f" {
  check 'skip   ;z    :=   x   +  y  ' '⇒ z := (x+y), {}
⇒ skip, {z → 0}'
}
