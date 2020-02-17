load ../../harness

@test "4fc42aefc293" {
  check 'if (¬(y     *   z     =p4   *     z)) then 
x   := z  + x  else y  := 1  +   W    ' '⇒ y := (1+W), {}
⇒ skip, {y → 1}'
}
