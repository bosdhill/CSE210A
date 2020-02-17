load ../../harness

@test "60416093f234" {
  check 'while false   ∨1     *     -3=    -4   ∨-3   =   4   -    1     do skip' '⇒ skip, {}'
}
