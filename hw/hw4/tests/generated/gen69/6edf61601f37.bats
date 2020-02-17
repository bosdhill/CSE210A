load ../../harness

@test "6edf61601f37" {
  check 'if (z   +   -1 =  3     -     y  ∨2   * 3    =    y  -  3)      then  

skip   else y   := -1  - s' '⇒ y := (-1-s), {}
⇒ skip, {y → -1}'
}
