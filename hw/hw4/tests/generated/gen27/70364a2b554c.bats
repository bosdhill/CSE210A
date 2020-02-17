load ../../harness

@test "70364a2b554c" {
  check 'if (-4    +   z     =-3    +2∧   true)   then skip else y :=-4  -   z ' '⇒ y := (-4-z), {}
⇒ skip, {y → -4}'
}
