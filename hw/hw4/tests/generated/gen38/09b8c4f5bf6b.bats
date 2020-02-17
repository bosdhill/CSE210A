load ../../harness

@test "09b8c4f5bf6b" {
  check 'while false    do Z  :=   1     *     -1  ' '⇒ skip, {}'
}
