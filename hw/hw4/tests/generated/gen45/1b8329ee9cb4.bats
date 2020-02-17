load ../../harness

@test "1b8329ee9cb4" {
  check 'z    :=  -3     +    z   ' '⇒ skip, {z → -3}'
}
