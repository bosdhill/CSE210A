load ../../harness

@test "5fc18acb6eb2" {
  check 'skip     ;z     :=  1 +    -2 ' '⇒ z := (1+-2), {}
⇒ skip, {z → -1}'
}
