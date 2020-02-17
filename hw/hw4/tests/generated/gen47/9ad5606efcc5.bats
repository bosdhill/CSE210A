load ../../harness

@test "9ad5606efcc5" {
  check 'if (¬true)  then    skip else skip ' '⇒ skip, {}'
}
