load ../../harness

@test "a06b75b81e0f" {
  check 'x:=  -1-   z;
   
z:=   y  -   w     ' '⇒ skip; z := (y-w), {x → -1}
⇒ z := (y-w), {x → -1}
⇒ skip, {x → -1, z → 0}'
}
