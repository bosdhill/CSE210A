load ../../harness

@test "a2c3ac5595c4" {
  check 'if (4   +    2  <  1  +y∧true)     then skip else 
 x :=     z     -   -3 ' '⇒ x := (z--3), {}
⇒ skip, {x → 3}'
}
