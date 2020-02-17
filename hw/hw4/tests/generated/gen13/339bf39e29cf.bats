load ../../harness

@test "339bf39e29cf" {
  check 'while -2   * 0<    -4 + 2     ∧   false    do    y:=y     +     -4' '⇒ skip, {}'
}
