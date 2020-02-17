load ../../harness

@test "0824803f1afa" {
  check 'if (true     ∨     2    + 0    <     1+   u)     then 
 
x    :=    3   -  2  else  skip     ' '⇒ x := (3-2), {}
⇒ skip, {x → 1}'
}
