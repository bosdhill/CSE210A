load ../../harness

@test "9caff9a629e7" {
  check 'skip   ;  z   :=   x   +    y' '⇒ z := (x+y), {}
⇒ skip, {z → 0}'
}
