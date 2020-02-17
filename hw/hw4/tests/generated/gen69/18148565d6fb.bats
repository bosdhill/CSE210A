load ../../harness

@test "18148565d6fb" {
  check 'x    :=  3  +4    ;skip  ' '⇒ skip; skip, {x → 7}
⇒ skip, {x → 7}'
}
