load ../../harness

@test "5254861e3129" {
  check 'while (¬(-2  +   2   =  y   *    y))    do skip  ' '⇒ skip, {}'
}
