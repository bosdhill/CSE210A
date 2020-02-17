load ../../harness

@test "41227b8902db" {
  check 'if (¬false) then skip     else 

skip ' '⇒ skip, {}'
}
