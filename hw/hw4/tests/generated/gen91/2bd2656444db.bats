load ../../harness

@test "2bd2656444db" {
  check 'if true  then   
skip      else skip   ' '⇒ skip, {}'
}
