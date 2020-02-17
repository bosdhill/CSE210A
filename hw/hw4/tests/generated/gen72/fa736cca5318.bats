load ../../harness

@test "fa736cca5318" {
  check 'if (false ∧     0     *   -4    <x   *    B6)    then 
 skip  else skip ' '⇒ skip, {}'
}
