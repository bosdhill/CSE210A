load ../../harness

@test "016e5b51f70e" {
  check 'z     :=     1    +    2   ;

x :=    2 - -1  ' '⇒ skip; x := (2--1), {z → 3}
⇒ x := (2--1), {z → 3}
⇒ skip, {x → 3, z → 3}'
}
