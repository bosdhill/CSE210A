load ../../harness

@test "b112c6d7d037" {
  check 'if (y  +   y  <    x     -   x  ∧    true)     then  skip      else   skip ' '⇒ skip, {}'
}
