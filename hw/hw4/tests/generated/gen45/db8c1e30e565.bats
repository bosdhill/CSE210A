load ../../harness

@test "db8c1e30e565" {
  check 'wc    := E    +     -4 ;  D:=x     ' '⇒ skip; D := x, {wc → -4}
⇒ D := x, {wc → -4}
⇒ skip, {D → 0, wc → -4}'
}
