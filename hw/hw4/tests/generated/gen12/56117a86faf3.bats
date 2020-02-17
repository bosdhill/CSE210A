load ../../harness

@test "56117a86faf3" {
  check 'if (¬(x<w     -   -3))      then skip    else  
 y:=    x    -    y ' '⇒ y := (x-y), {}
⇒ skip, {y → 0}'
}
