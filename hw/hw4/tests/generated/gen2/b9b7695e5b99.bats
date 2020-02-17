load ../../harness

@test "b9b7695e5b99" {
  check 'lq     :=     z   *a  ;
 

z:=  F  +2' '⇒ skip; z := (F+2), {lq → 0}
⇒ z := (F+2), {lq → 0}
⇒ skip, {lq → 0, z → 2}'
}
