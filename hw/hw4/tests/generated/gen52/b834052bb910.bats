load ../../harness

@test "b834052bb910" {
  check 'while false∧  x     -     1     <    -3 + 2 do 
    z    := -4   ' '⇒ skip, {}'
}
