load ../../harness

@test "e09cf3a8e9db" {
  check 'if (0  +   0  <   x- x  ∧0   *  1     <    x  +     -1)     then skip      else skip' '⇒ skip, {}'
}
