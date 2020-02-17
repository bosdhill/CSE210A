load ../../harness

@test "8c6382108d9d" {
  check 'while (¬true) do  
  
skip' '⇒ skip, {}'
}
