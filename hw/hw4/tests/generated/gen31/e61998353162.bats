load ../../harness

@test "e61998353162" {
  check 'if (¬(-4     * y   =  y +     2))   then skip else    

y    :=    -1  +-2  ' '⇒ skip, {}'
}
