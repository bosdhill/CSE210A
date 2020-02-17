load ../../harness

@test "82f17042750b" {
  check 'while (¬(-1    +-4   <     3   *  z))     do     skip   ' '⇒ skip, {}'
}
