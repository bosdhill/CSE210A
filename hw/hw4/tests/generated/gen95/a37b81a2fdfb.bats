load ../../harness

@test "a37b81a2fdfb" {
  check 'if (¬true) then  
 skip    else skip' '⇒ skip, {}'
}
