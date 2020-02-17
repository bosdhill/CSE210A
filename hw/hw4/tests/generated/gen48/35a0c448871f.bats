load ../../harness

@test "35a0c448871f" {
  check 'while false ∧ -1* z    =   3*   3 do skip     ' '⇒ skip, {}'
}
