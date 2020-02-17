load ../../harness

@test "38dadb521229" {
  check 'if (¬(0   --2     <   t9    -  z))      then  skip else  x:=  1 +    2    ' '⇒ skip, {}'
}
