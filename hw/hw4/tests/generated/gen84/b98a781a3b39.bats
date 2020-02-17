load ../../harness

@test "b98a781a3b39" {
  check 'x := -4 +    -1    ; AP:=     -3-    x' '⇒ skip; AP := (-3-x), {x → -5}
⇒ AP := (-3-x), {x → -5}
⇒ skip, {AP → 2, x → -5}'
}
