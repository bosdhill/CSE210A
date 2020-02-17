load ../../harness

@test "55e58236fbf6" {
  check 'x     :=  y *   y  ;r    :=-1   *     0 ' '⇒ skip; r := (-1*0), {x → 0}
⇒ r := (-1*0), {x → 0}
⇒ skip, {r → 0, x → 0}'
}
