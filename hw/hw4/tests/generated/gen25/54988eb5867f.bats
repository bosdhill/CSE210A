load ../../harness

@test "54988eb5867f" {
  check 'while false ∨  false    do  
skip  ' '⇒ skip, {}'
}
