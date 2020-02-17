load ../../harness

@test "79a80015559e" {
  check 'z   := 2     +    z    ' '⇒ skip, {z → 2}'
}
