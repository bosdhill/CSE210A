load ../../harness

@test "d763fcfefdc9" {
  check 'x   :=    1  *4  ;x     :=     x     --4    ' '⇒ skip; x := (x--4), {x → 4}
⇒ x := (x--4), {x → 4}
⇒ skip, {x → 8}'
}
