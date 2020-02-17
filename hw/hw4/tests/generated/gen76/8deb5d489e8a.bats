load ../../harness

@test "8deb5d489e8a" {
  check 'z:= x   +    -2     ' '⇒ skip, {z → -2}'
}
