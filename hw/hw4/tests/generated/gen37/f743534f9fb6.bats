load ../../harness

@test "f743534f9fb6" {
  check 'if (¬(z*   1  =     2    -  z))     then skip else  x   := -2     +     G' '⇒ skip, {}'
}
