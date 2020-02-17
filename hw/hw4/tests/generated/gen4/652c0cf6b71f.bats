load ../../harness

@test "652c0cf6b71f" {
  check 'if (rx    <     -2    * 2)   then  
 skip      else      skip  ' '⇒ skip, {}'
}
