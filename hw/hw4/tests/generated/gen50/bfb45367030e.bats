load ../../harness

@test "bfb45367030e" {
  check 'if (¬(1 +   -2     <y *    x))     then 
RD:=    y     -c3     else x  := -1  ' '⇒ x := -1, {}
⇒ skip, {x → -1}'
}
