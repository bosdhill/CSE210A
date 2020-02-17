load ../../harness

@test "aae95e127086" {
  check 'while 0   *     x =     y   -   -3     ∨   x    <    0    +   z      do  x  :=  y    ' '⇒ skip, {}'
}
