load ../../harness

@test "1a9a0bd2b584" {
  check 'if (¬true) then skip     else skip     ' '⇒ skip, {}'
}
