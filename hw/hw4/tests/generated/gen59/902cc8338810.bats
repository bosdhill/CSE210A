load ../../harness

@test "902cc8338810" {
  check 'if (1    -   -1   <    1   -  y    ∨  true)     then   skip else x:=    x     +   y' '⇒ skip, {}'
}
