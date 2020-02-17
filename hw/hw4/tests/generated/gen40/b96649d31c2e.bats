load ../../harness

@test "b96649d31c2e" {
  check 'if (-3   - 0=    z    *    y∨  z    +     -4 <M     + z)     then   skip   else x   :=  4    ' '⇒ skip, {}'
}
