load ../../harness

@test "cec718b25e86" {
  check 'if (¬true) then      skip else skip' '⇒ skip, {}'
}
