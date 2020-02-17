load ../../harness

@test "822d6e8359c7" {
  check 'y:=  3+-4  ; 
  x:=     x --4 ' '⇒ skip; x := (x--4), {y → -1}
⇒ x := (x--4), {y → -1}
⇒ skip, {x → 4, y → -1}'
}
