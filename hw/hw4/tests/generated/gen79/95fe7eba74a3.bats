load ../../harness

@test "95fe7eba74a3" {
  check 'if (false   ∨    x    +  3    <y     *   2)    then   
skip else 
skip   ' '⇒ skip, {}'
}
