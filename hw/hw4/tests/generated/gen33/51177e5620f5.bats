load ../../harness

@test "51177e5620f5" {
  check 'if true      then 
 skip   else 
 

skip   ' '⇒ skip, {}'
}
