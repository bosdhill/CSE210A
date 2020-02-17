load ../../harness

@test "7fc9bf2f7f25" {
  check 'if (¬false)     then 
skip else  

skip    ' '⇒ skip, {}'
}
