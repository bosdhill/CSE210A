load ../../harness

@test "8db9582a099b" {
  check 'if (false     ∧     y -    x <    -3  +-3) then 
  skip      else   skip  ' '⇒ skip, {}'
}
