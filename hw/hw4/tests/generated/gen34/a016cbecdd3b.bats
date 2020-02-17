load ../../harness

@test "a016cbecdd3b" {
  check 'if (true    ∧   true) then skip   else skip ' '⇒ skip, {}'
}
