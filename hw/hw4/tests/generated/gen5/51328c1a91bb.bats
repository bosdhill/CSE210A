load ../../harness

@test "51328c1a91bb" {
  check 'while false ∨z    +x  <     1   +-2  do   skip    ' '⇒ skip, {}'
}
