load ../../harness

@test "7d87225c236a" {
  check 'while (¬(y     +x  =   -1 * x))     do   y    :=z     -     3   ' '⇒ skip, {}'
}
