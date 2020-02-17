load ../../harness

@test "bad022b15acc" {
  check 'if (¬false) then   
skip     else skip ' '⇒ skip, {}'
}
