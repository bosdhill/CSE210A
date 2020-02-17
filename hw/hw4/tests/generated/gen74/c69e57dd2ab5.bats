load ../../harness

@test "c69e57dd2ab5" {
  check 'kY:= ym -    2    ;x:=     x   + x' '⇒ skip; x := (x+x), {kY → -2}
⇒ x := (x+x), {kY → -2}
⇒ skip, {kY → -2, x → 0}'
}
