load ../../harness

@test "42aa17d8e81b" {
  check 'z     :=   -1     -y     ;  

x:=     y    -  z' '⇒ skip; x := (y-z), {z → -1}
⇒ x := (y-z), {z → -1}
⇒ skip, {x → 1, z → -1}'
}
