load ../../harness

@test "b6341256b339" {
  check 'while x    +    z    =   Hz     ∧  false    do    x := y     * 0     ' '⇒ skip, {}'
}
