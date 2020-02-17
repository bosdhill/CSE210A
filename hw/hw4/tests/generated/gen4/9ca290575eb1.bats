load ../../harness

@test "9ca290575eb1" {
  check 'x     := -2    + z     ;

z :=    ax     *  3' '⇒ skip; z := (ax*3), {x → -2}
⇒ z := (ax*3), {x → -2}
⇒ skip, {x → -2, z → 0}'
}
