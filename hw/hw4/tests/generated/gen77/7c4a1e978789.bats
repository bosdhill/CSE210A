load ../../harness

@test "7c4a1e978789" {
  check 'while -1*  -2    <y    -     z do    y  := y     * z' '⇒ skip, {}'
}
