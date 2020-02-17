load ../../harness

@test "3e127f3e19d4" {
  check 'if (true  ∧  y     -   z    <  0   -     -3)   then 
 skip  else z :=-2 +y   ' '⇒ skip, {}'
}
