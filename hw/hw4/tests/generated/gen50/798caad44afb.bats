load ../../harness

@test "798caad44afb" {
  check 'while false  ∧  true    do A0  :=    y  *1   ' '⇒ skip, {}'
}
