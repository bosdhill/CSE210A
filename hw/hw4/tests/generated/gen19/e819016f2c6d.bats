load ../../harness

@test "e819016f2c6d" {
  check 'if (¬false) then skip else skip' '⇒ skip, {}'
}
