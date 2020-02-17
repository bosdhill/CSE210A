load ../../harness

@test "f126047ae49d" {
  check 'if (y    -y <    -2*x  ∧     true)   then  skip     else skip  ' '⇒ skip, {}'
}
