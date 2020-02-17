load ../../harness

@test "74b2f570385f" {
  check 'if (y =y     -    x)     then   
skip    else   x   := 4   ' '⇒ skip, {}'
}
