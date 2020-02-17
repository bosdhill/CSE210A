load ../../harness

@test "03b35b546023" {
  check 'if (¬(4   + -2 <  -3  +  S)) then 
 skip    else DP  := y   -  2  ' '⇒ skip, {}'
}
