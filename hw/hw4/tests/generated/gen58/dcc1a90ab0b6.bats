load ../../harness

@test "dcc1a90ab0b6" {
  check 'u    :=  y-xd ;y   :=  3  +     2    ' '⇒ skip; y := (3+2), {u → 0}
⇒ y := (3+2), {u → 0}
⇒ skip, {u → 0, y → 5}'
}
