load ../../harness

@test "f7056e1c627d" {
  check 'if (¬false)     then  
skip   else skip' '⇒ skip, {}'
}
