load ../../harness

@test "4f57acc2e461" {
  check 'if (false    ∨ y    + -4    <     z *    1)     then  skip      else  skip' '⇒ skip, {}'
}
