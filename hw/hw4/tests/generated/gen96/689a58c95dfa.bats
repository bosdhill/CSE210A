load ../../harness

@test "689a58c95dfa" {
  check 'if true then   
 skip  else skip' '⇒ skip, {}'
}
