load ../../harness

@test "df27ba970728" {
  check 'skip  ; x    :=    y+   -2' '⇒ x := (y+-2), {}
⇒ skip, {x → -2}'
}
