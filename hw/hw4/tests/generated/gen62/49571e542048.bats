load ../../harness

@test "49571e542048" {
  check 'skip; z   :=  z ' '⇒ z := z, {}
⇒ skip, {z → 0}'
}
