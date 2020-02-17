load ../../harness

@test "28017afc19f2" {
  check 'if (¬true) then  skip else  skip     ' '⇒ skip, {}'
}
