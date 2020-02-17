load ../../harness

@test "9d17728eb894" {
  check 'x     :=    x *  z;  j :=    z' '⇒ skip; j := z, {x → 0}
⇒ j := z, {x → 0}
⇒ skip, {j → 0, x → 0}'
}
