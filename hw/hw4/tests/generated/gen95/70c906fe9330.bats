load ../../harness

@test "70c906fe9330" {
  check 'if (¬true)   then 
  skip      else skip' '⇒ skip, {}'
}
