load ../../harness

@test "7b966c960017" {
  check 'yX:= -1   +    -2 ' '⇒ skip, {yX → -3}'
}
