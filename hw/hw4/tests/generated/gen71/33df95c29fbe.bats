load ../../harness

@test "33df95c29fbe" {
  check 'x := x *1    ;


o    :=x    *     -3' '⇒ skip; o := (x*-3), {x → 0}
⇒ o := (x*-3), {x → 0}
⇒ skip, {o → 0, x → 0}'
}
