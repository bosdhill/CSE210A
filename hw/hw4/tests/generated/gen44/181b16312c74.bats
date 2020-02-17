load ../../harness

@test "181b16312c74" {
  check 'while (¬true)  do 
x :=  x  -  z  ' '⇒ skip, {}'
}
