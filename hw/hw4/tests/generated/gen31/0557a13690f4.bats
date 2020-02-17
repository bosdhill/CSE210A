load ../../harness

@test "0557a13690f4" {
  check 'p :=x-    2; 
  y  :=     -3     -   z ' '⇒ skip; y := (-3-z), {p → -2}
⇒ y := (-3-z), {p → -2}
⇒ skip, {p → -2, y → -3}'
}
