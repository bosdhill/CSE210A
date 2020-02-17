load ../../harness

@test "20d36e46d3f9" {
  check 'if (true   ∨ false)  then 
 
skip     else    skip' '⇒ skip, {}'
}
