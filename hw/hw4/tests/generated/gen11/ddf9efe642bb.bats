load ../../harness

@test "ddf9efe642bb" {
  check 'while y -   -2   =    3    *  z    ∧  x=  y   do 
  y     :=  3    *    y    ' '⇒ skip, {}'
}
