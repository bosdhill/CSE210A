load ../../harness

@test "19a9f1b46346" {
  check 'while (¬true)   do   skip ' '⇒ skip, {}'
}
