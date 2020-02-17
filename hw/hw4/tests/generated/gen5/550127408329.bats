load ../../harness

@test "550127408329" {
  check 'y     := I8  +  -3     ' '⇒ skip, {y → -3}'
}
