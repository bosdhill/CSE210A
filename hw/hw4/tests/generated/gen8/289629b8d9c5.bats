load ../../harness

@test "289629b8d9c5" {
  check 'if (¬true)   then skip   else     skip   ' '⇒ skip, {}'
}
