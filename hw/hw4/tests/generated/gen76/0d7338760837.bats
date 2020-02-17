load ../../harness

@test "0d7338760837" {
  check 'if (¬true)      then  skip    else skip ' '⇒ skip, {}'
}
