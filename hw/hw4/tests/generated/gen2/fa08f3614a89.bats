load ../../harness

@test "fa08f3614a89" {
  check 'while false ∨     -3    * -2  <    y     do     skip    ' '⇒ skip, {}'
}
