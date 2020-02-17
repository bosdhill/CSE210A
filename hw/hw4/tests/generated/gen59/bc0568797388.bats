load ../../harness

@test "bc0568797388" {
  check 'if (y +z     = -2  +     x∨-4     *   y  = 4   *0)   then skip   else    x    :=     4 -  0    ' '⇒ skip, {}'
}
