load ../../harness

@test "e465b1171b71" {
  check 'Sn    :=     Z   *   y ;x  :=    z +    x ' '⇒ skip; x := (z+x), {Sn → 0}
⇒ x := (z+x), {Sn → 0}
⇒ skip, {Sn → 0, x → 0}'
}
