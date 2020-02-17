load ../../harness

@test "a4acd5894a27" {
  check 'if (¬true)     then z:=     y *  2    else  
hx    :=   x   +  -1' '⇒ hx := (x+-1), {}
⇒ skip, {hx → -1}'
}
