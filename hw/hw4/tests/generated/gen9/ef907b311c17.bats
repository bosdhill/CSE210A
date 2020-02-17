load ../../harness

@test "ef907b311c17" {
  check 'if (1    *  x    =  4- -2  ∨   -1     -  4    =     1     +   z)     then y    :=    y   *   y      else 
  y:=  1 *    y     ' '⇒ y := (1*y), {}
⇒ skip, {y → 0}'
}
