load ../../harness

@test "6b6633cc3e5c" {
  check 'if (¬true)     then  skip else  
skip     ' '⇒ skip, {}'
}
