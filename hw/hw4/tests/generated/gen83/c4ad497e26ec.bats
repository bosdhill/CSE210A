load ../../harness

@test "c4ad497e26ec" {
  check 'if (false ∨   -1-  z  < y   + 4)  then  skip   else   skip  ' '⇒ skip, {}'
}
