load ../../harness

@test "e66c53204cfc" {
  check 'if (y    =    y     +     z    ∨     -1   =    z+    z)   then skip     else   
x  :=     2 *  y     ' '⇒ skip, {}'
}
