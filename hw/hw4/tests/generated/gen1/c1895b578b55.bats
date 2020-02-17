load ../../harness

@test "c1895b578b55" {
  check 'x:=    x  +     4  ;


x    :=x *  0' '⇒ skip; x := (x*0), {x → 4}
⇒ x := (x*0), {x → 4}
⇒ skip, {x → 0}'
}
