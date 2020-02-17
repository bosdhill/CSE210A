load ../../harness

@test "7c599eca406a" {
  check 'if (true  ∨   -3 *-1  <    -1  -    x)      then   
skip     else   skip     ' '⇒ skip, {}'
}
