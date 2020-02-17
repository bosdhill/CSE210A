load ../../harness

@test "5bb4855e8b75" {
  check 'if (-2   =y  ∨  z   -y    =   -2)      then    x    :=    y  +   y   else  
YU := x   - z    ' '⇒ YU := (x-z), {}
⇒ skip, {YU → 0}'
}
