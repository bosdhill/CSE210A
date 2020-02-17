load ../../harness

@test "1ffa007b2077" {
  check 'y:=  z    *    -2 ' '⇒ skip, {y → 0}'
}
