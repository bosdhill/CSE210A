load ../../harness

@test "aeef47765e71" {
  check 'skip     ;


 z:=  1 +     -1;  z:=   x     *  0     ' '⇒ z := (1+-1); z := (x*0), {}
⇒ skip; z := (x*0), {z → 0}
⇒ z := (x*0), {z → 0}
⇒ skip, {z → 0}'
}
