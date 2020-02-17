load ../../harness

@test "f1aa5fb2fde8" {
  check 'if (true     ∨false)   then 

skip else skip    ' '⇒ skip, {}'
}
