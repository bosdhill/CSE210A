load ../../harness

@test "adde7fd4dde4" {
  check 'if (false ∧    false)     then  skip      else  
skip   ' '⇒ skip, {}'
}
