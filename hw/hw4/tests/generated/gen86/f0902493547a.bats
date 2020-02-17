load ../../harness

@test "f0902493547a" {
  check 'z:=     y-    1    ;
 
z:=  y     +  -4 ' '⇒ skip; z := (y+-4), {z → -1}
⇒ z := (y+-4), {z → -1}
⇒ skip, {z → -4}'
}
