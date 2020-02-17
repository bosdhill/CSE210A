load ../../harness

@test "91016da47ff2" {
  check 'if (x     *   y < 1+x  ∧     true)  then  skip    else   y    := -2   -m' '⇒ skip, {}'
}
