load ../../harness

@test "7a020dbe20b4" {
  check 'while (¬(A3   -   x     <    1)) do    y     :=    -3   +    c ' '⇒ skip, {}'
}
