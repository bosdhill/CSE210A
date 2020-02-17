load ../../harness

@test "6ec03ceebac8" {
  check 'if (¬(3     +-4  < 1  + x))     then 
   skip else  
 skip     ' '⇒ skip, {}'
}
