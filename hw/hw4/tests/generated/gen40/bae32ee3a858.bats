load ../../harness

@test "bae32ee3a858" {
  check 'x    :=    -4 +   x  ;
 z    :=    x    *     x    ' '⇒ skip; z := (x*x), {x → -4}
⇒ z := (x*x), {x → -4}
⇒ skip, {x → -4, z → 16}'
}
