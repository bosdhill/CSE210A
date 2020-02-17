load ../../harness

@test "d2c5e0009761" {
  check 'if (¬false) then 
   skip    else 
 
skip' '⇒ skip, {}'
}
