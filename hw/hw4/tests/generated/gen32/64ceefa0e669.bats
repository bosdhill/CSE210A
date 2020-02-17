load ../../harness

@test "64ceefa0e669" {
  check 'y   :=  z   +  -3     ' '⇒ skip, {y → -3}'
}
