load ../../harness

@test "2cce574fd23c" {
  check 'if (¬true)     then skip     else 
skip ' '⇒ skip, {}'
}
