load ../../harness

@test "f2300480ab18" {
  check 'if false then  skip    else  skip  ' '⇒ skip, {}'
}
