load ../../harness

@test "d246df9e2873" {
  check 'while false ∧     0    *   G     =    3    +-4    do skip     ' '⇒ skip, {}'
}
