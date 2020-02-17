load ../../harness

@test "f52a49c58e5b" {
  check 'x:=z+   -1;  
 z     :=  x -  D    ' '⇒ skip; z := (x-D), {x → -1}
⇒ z := (x-D), {x → -1}
⇒ skip, {x → -1, z → -1}'
}
