load ../../harness

@test "36ade2c73bcd" {
  check 'z  :=  x  +     4; 

x    :=    2   --1     ' '⇒ skip; x := (2--1), {z → 4}
⇒ x := (2--1), {z → 4}
⇒ skip, {x → 3, z → 4}'
}
