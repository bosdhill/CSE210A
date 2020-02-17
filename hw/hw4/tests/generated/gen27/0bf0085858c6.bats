load ../../harness

@test "0bf0085858c6" {
  check 'while 4 -  Q <  0    +   x   ∨     2    +   x  <    1   *   y      do 
z  :=y     + 1   ' '⇒ skip, {}'
}
