load ../../harness

@test "611a99bf5459" {
  check 'x   :=   x   *D  ; ZS :=    n    *2  ' '⇒ skip; ZS := (n*2), {x → 0}
⇒ ZS := (n*2), {x → 0}
⇒ skip, {ZS → 0, x → 0}'
}
