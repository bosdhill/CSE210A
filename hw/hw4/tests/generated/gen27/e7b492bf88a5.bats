load ../../harness

@test "e7b492bf88a5" {
  check 'while 3  + 0   =    x +   x∨   false     do W  :=  -2 - z' '⇒ skip, {}'
}
