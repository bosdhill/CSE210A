load ../../harness

@test "5272d3927626" {
  check 'x     :=3     -     -3  ;
  bz:=     3-   -3' '⇒ skip; bz := (3--3), {x → 6}
⇒ bz := (3--3), {x → 6}
⇒ skip, {bz → 6, x → 6}'
}
