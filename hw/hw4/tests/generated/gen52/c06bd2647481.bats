load ../../harness

@test "c06bd2647481" {
  check 'while 3 <  3  - y do skip    ' '⇒ skip, {}'
}
