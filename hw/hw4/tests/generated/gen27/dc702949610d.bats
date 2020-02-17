load ../../harness

@test "dc702949610d" {
  check 'z   :=   1  +     x;z   := x     -    2   ' '⇒ skip; z := (x-2), {z → 1}
⇒ z := (x-2), {z → 1}
⇒ skip, {z → -2}'
}
