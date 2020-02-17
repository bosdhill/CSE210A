load ../../harness

@test "ef20a3980b9b" {
  check 'if (¬true)  then 
skip else 
skip' '⇒ skip, {}'
}
