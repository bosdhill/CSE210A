load ../../harness

@test "9d090f2f8edf" {
  check 'if (¬(x    +     z     <     -4 +  E))      then  
y    :=   Sc  *    x  else z:=    y    *   y  ' '⇒ y := (Sc*x), {}
⇒ skip, {y → 0}'
}
