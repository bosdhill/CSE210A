load ../../harness

@test "f7206274845d" {
  check 'if (x +y=z)      then  skip      else  skip  ' '⇒ skip, {}'
}
