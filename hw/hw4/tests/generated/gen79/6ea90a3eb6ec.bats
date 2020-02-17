load ../../harness

@test "6ea90a3eb6ec" {
  check 'while 0   <  z -z     do skip' '⇒ skip, {}'
}
