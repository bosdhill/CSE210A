load ../../harness

@test "297c7743a5c6" {
  check 'if (-1     +   N=4  *   -3∨    z    +   -4    = y +x)      then    skip  else   ht    :=   1  -     -3  ' '⇒ ht := (1--3), {}
⇒ skip, {ht → 4}'
}
