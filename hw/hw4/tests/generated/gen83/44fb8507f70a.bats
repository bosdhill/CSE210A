load ../../harness

@test "44fb8507f70a" {
  check 'while 4  +x   =     y--1  ∧true   do  skip  ' '⇒ skip, {}'
}
