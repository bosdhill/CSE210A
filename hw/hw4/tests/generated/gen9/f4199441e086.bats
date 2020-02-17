load ../../harness

@test "f4199441e086" {
  check 'if (false     ∨ true)     then  
 skip    else 

skip  ' '⇒ skip, {}'
}
