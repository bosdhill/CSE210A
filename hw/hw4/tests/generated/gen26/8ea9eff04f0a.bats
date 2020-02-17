load ../../harness

@test "8ea9eff04f0a" {
  check 'if (¬false)      then skip else  e   := 3  -    Vt    ' '⇒ skip, {}'
}
