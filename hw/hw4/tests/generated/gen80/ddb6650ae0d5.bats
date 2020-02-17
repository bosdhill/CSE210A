load ../../harness

@test "ddb6650ae0d5" {
  check 'if (3    *  -4 <  0   +     y)      then 
  skip      else x :=     3    *  -2     ' '⇒ skip, {}'
}
