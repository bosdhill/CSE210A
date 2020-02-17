load ../../harness

@test "d835097bf5ae" {
  check 'z :=  x1+x     ;

z  := z    *     w     ' '⇒ skip; z := (z*w), {z → 0}
⇒ z := (z*w), {z → 0}
⇒ skip, {z → 0}'
}
