load ../../harness

@test "355e641ed1ca" {
  check 'if (¬(3+ B    < 4     *  -4))      then  y     :=  x     -    2   else 
skip  ' '⇒ y := (x-2), {}
⇒ skip, {y → -2}'
}
