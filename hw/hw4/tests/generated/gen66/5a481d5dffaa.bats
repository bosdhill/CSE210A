load ../../harness

@test "5a481d5dffaa" {
  check 'while -4+     2   =   4     *     x do  skip    ' '⇒ skip, {}'
}
