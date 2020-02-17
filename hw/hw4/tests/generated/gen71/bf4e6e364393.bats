load ../../harness

@test "bf4e6e364393" {
  check 'x:=   o-  3    ; 
   D     :=  x    --2' '⇒ skip; D := (x--2), {x → -3}
⇒ D := (x--2), {x → -3}
⇒ skip, {D → -1, x → -3}'
}
