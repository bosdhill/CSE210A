load ../../harness

@test "5c4ab59fd7af" {
  check 'if (-4   *   x=-3  *     4∧   true)    then  skip  else   
 x  :=-2   ' '⇒ x := -2, {}
⇒ skip, {x → -2}'
}
