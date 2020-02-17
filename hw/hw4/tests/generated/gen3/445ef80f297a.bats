load ../../harness

@test "445ef80f297a" {
  check 'while x +     x    =  x    -   -4∧false    do  x  :=     y+   3   ' '⇒ skip, {}'
}
