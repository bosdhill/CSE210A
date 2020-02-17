load ../../harness

@test "6be9626aa4b7" {
  check 'if (¬(-2    +  -4    < 3  -   1))    then 
skip   else 
skip     ' '⇒ skip, {}'
}
