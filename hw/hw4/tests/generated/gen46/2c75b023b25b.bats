load ../../harness

@test "2c75b023b25b" {
  check 'if (-4   *   z    <    x  -3   ∨  true)  then  skip      else z   := -2     ' '⇒ skip, {}'
}
