load ../../harness

@test "24bd81a3d53c" {
  check 'if (1  +     y    =     y *  z)     then 
 skip     else x  :=     -4 ' '⇒ x := -4, {}
⇒ skip, {x → -4}'
}
