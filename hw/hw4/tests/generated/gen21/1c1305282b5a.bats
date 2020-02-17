load ../../harness

@test "1c1305282b5a" {
  check 'x    :=    z +     -2     ;
 
 PG   :=z+     1  ' '⇒ skip; PG := (z+1), {x → -2}
⇒ PG := (z+1), {x → -2}
⇒ skip, {PG → 1, x → -2}'
}
