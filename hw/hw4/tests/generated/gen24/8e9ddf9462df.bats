load ../../harness

@test "8e9ddf9462df" {
  check 'x    :=    z  ;



x:=  0     -    y   ' '⇒ skip; x := (0-y), {x → 0}
⇒ x := (0-y), {x → 0}
⇒ skip, {x → 0}'
}
