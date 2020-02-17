load ../../harness

@test "60daa5d7fe7d" {
  check 'if (F   + -4    < F   ∧    true)  then   
skip  else skip   ' '⇒ skip, {}'
}
