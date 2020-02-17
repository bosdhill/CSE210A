load ../../harness

@test "04b8c0c88f79" {
  check 'while x  +     y     <    z  do 
skip' '⇒ skip, {}'
}
