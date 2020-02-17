load ../../harness

@test "76f7b87ff0f7" {
  check 'if (¬(1 +  oD    <   -1  -   x))    then y  :=     y    +   4 else 
z  :=    2    *    -2   ' '⇒ y := (y+4), {}
⇒ skip, {y → 4}'
}
