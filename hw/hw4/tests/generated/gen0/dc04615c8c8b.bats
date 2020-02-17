load ../../harness

@test "dc04615c8c8b" {
  check 'x  :=  -2  * -1;skip ' '⇒ skip; skip, {x → 2}
⇒ skip, {x → 2}'
}
