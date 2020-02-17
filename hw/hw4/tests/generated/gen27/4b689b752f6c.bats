load ../../harness

@test "4b689b752f6c" {
  check 'if (0    * z <  4     * y)     then  y :=  y    else 
 skip   ' '⇒ skip, {}'
}
