load ../../harness

@test "d1387d66802f" {
  check 'if (¬true)     then skip  else skip ' '⇒ skip, {}'
}
