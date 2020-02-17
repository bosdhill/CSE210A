load ../../harness

@test "d2f52e49fc57" {
  check 'if (¬(1  <    2     + 4))  then  skip  else  skip  ' '⇒ skip, {}'
}
