load ../../harness

@test "05dfb6abc504" {
  check 'z  :=     y +y    ;

z    :=   4 -  -4 ' '⇒ skip; z := (4--4), {z → 0}
⇒ z := (4--4), {z → 0}
⇒ skip, {z → 8}'
}
