load ../../harness

@test "400c7d22d733" {
  check 'if (x --4   <     -3 * 4)     then 
x  :=z  - y     else 
skip' '⇒ skip, {}'
}
