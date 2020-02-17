load ../../harness

@test "056078989019" {
  check 'if (¬true)     then  
v := 0   +    -3      else skip    ' '⇒ skip, {}'
}
