load ../../harness

@test "ae60cb1f7ca2" {
  check 'if (false    ∧   false) then 
 skip    else    skip    ' '⇒ skip, {}'
}
