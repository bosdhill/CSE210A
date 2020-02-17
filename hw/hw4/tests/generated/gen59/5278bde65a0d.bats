load ../../harness

@test "5278bde65a0d" {
  check 'while 1 +   y     <    y -DX     ∨false      do skip     ' '⇒ skip, {}'
}
