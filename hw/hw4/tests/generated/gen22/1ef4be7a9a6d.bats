load ../../harness

@test "1ef4be7a9a6d" {
  check 'z    :=     2+-4    ;


x     :=  y  +     z     ' '⇒ skip; x := (y+z), {z → -2}
⇒ x := (y+z), {z → -2}
⇒ skip, {x → -2, z → -2}'
}
