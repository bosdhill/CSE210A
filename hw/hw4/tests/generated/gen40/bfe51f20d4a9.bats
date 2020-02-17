load ../../harness

@test "bfe51f20d4a9" {
  check 'if (1     *  y   <     -2+   y)    then  skip  else 


y    :=   z     *4     ' '⇒ y := (z*4), {}
⇒ skip, {y → 0}'
}
