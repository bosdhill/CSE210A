load ../../harness

@test "c767d44175da" {
  check 'z:=    4  -  x;  skip  ' '⇒ skip; skip, {z → 4}
⇒ skip, {z → 4}'
}
