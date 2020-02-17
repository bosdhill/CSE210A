load ../../harness

@test "04d30f09474d" {
  check 'if (¬false)  then  skip      else    skip ' '⇒ skip, {}'
}
