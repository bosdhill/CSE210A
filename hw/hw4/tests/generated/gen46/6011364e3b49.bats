load ../../harness

@test "6011364e3b49" {
  check 'if (y    -    y     =   y   *   1∨0  -3   =     3    -  -4)    then     skip      else z  :=   y    -    -2  ' '⇒ skip, {}'
}
