load ../../harness

@test "51741a1b51a3" {
  check 'L    :=3 *  4    ;  
x  :=  qd  *-2  ' '⇒ skip; x := (qd*-2), {L → 12}
⇒ x := (qd*-2), {L → 12}
⇒ skip, {L → 12, x → 0}'
}
