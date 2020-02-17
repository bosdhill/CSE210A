load ../../harness

@test "b5f1672b822b" {
  check 'while false ∧     2  *  -4<   0   -   4    do  z     :=   z +   z' '⇒ skip, {}'
}
