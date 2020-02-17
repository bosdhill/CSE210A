load ../../harness

@test "c09dcdeb82b6" {
  check 'if (false    ∨   x    *    x  =   -2    *    y)     then C8   :=y   -  3    else  
skip' '⇒ C8 := (y-3), {}
⇒ skip, {C8 → -3}'
}
