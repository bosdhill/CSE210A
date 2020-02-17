load ../../harness

@test "281a40b78ec0" {
  check 'if (z     *  x    <     z   -     -2 ∨     false)  then 
 
skip   else z :=  x  -     4     ' '⇒ skip, {}'
}
