load ../../harness

@test "b80eba4730d6" {
  check 'if (false ∧   false)  then 
 skip   else skip  ' '⇒ skip, {}'
}
