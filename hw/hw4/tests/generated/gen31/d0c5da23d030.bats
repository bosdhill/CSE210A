load ../../harness

@test "d0c5da23d030" {
  check 'if (¬(G     -     V  <3    -   y))    then x   :=    4 *   -4     else  y  :=    y -    4    ' '⇒ y := (y-4), {}
⇒ skip, {y → -4}'
}
