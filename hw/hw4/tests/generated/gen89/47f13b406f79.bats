load ../../harness

@test "47f13b406f79" {
  check 'while x   -    UL   <     x     +-4   ∧  true      do z    :=  y  -     2   ' '⇒ skip, {}'
}
