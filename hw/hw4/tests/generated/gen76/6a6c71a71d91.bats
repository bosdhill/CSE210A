load ../../harness

@test "6a6c71a71d91" {
  check 'if (¬(x   +  x  =  VO  *    x))      then z:=   z   *  y      else   z :=x  -  x ' '⇒ z := (x-x), {}
⇒ skip, {z → 0}'
}
