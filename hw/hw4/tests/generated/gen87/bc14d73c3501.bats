load ../../harness

@test "bc14d73c3501" {
  check 'if (y   -   -1 = -4+     x)     then q   := g+   z     else skip    ' '⇒ skip, {}'
}
