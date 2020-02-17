load ../../harness

@test "9599cd2329f7" {
  check 'z:=  y   +     y   ;
 z   :=     y   - -4' '⇒ skip; z := (y--4), {z → 0}
⇒ z := (y--4), {z → 0}
⇒ skip, {z → 4}'
}
