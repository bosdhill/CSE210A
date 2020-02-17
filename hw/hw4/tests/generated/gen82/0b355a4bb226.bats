load ../../harness

@test "0b355a4bb226" {
  check 'x  :=     -3  +  4  ;    x    :=    3     + z' '⇒ skip; x := (3+z), {x → 1}
⇒ x := (3+z), {x → 1}
⇒ skip, {x → 3}'
}
