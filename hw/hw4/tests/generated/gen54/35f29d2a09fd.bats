load ../../harness

@test "35f29d2a09fd" {
  check 'if (y     *     y   < 1   -     -3∨     false)  then  
skip  else skip     ' '⇒ skip, {}'
}
