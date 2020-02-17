load ../../harness

@test "202800edd0ec" {
  check 'if (true ∨   x     =   -3    * -3)     then    skip else skip ' '⇒ skip, {}'
}
