load ../../harness

@test "44b9e30c47dd" {
  check 'if true  then    skip      else VF   :=     i *    1    ' '⇒ skip, {}'
}
