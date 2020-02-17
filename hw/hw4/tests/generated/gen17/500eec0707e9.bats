load ../../harness

@test "500eec0707e9" {
  check 'while false    ∨  false    do  skip' '⇒ skip, {}'
}
