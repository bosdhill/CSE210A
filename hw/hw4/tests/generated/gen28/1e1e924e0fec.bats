load ../../harness

@test "1e1e924e0fec" {
  check 'while (¬true) do    x    :=     3     * q  ' '⇒ skip, {}'
}
