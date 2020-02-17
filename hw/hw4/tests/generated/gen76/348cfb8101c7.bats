load ../../harness

@test "348cfb8101c7" {
  check 'while false  do 
k  :=x - x     ' '⇒ skip, {}'
}
