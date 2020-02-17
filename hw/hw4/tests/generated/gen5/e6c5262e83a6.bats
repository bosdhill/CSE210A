load ../../harness

@test "e6c5262e83a6" {
  check 'while y     -x    =   3    ∨   false do  skip' '⇒ skip, {}'
}
