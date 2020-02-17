load ../../harness

@test "6383689b5298" {
  check 'f  :=   y    +   A  ;
 x  :=-1  -   z   ' '⇒ skip; x := (-1-z), {f → 0}
⇒ x := (-1-z), {f → 0}
⇒ skip, {f → 0, x → -1}'
}
