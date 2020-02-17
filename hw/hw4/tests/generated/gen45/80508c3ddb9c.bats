load ../../harness

@test "80508c3ddb9c" {
  check 'if (x    *  -3     =     U8   --1  ∨pq - y=z  - z)   then 
skip      else skip' '⇒ skip, {}'
}
