load ../../harness

@test "3516bb851028" {
  check 'if (¬false)  then skip     else skip     ' '⇒ skip, {}'
}
