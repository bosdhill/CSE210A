load ../../harness

@test "573d6083a6df" {
  check 'if (1   <     y  + 3 +  4   ∨     3   +    -3     <     y-  z)   then skip   else   
z     :=     -4 *   y ' '⇒ skip, {}'
}
