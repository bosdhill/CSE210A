load ../../harness

@test "26e36a83e2c6" {
  check 'if (false∧ true)  then  skip     else  skip' '⇒ skip, {}'
}
