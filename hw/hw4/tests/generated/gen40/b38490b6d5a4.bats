load ../../harness

@test "b38490b6d5a4" {
  check 'if (3     * y    =  4 - z   ∨   true)     then  skip     else  y     :=  -3  +     -1   ' '⇒ skip, {}'
}
