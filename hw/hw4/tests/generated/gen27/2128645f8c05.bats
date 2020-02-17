load ../../harness

@test "2128645f8c05" {
  check 'if (¬(z  -     C   <   4))     then  
 skip    else y  := -1   ' '⇒ y := -1, {}
⇒ skip, {y → -1}'
}
