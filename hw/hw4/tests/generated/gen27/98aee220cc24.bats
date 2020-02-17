load ../../harness

@test "98aee220cc24" {
  check 'if true      then skip      else skip' '⇒ skip, {}'
}
