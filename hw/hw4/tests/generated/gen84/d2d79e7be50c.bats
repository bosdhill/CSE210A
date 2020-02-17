load ../../harness

@test "d2d79e7be50c" {
  check 'if (-3 + z     =  1  -    x)    then y :=   y -   x  else  x    :=     3     -     4    ' '⇒ x := (3-4), {}
⇒ skip, {x → -1}'
}
