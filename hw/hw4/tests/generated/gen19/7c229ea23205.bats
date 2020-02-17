load ../../harness

@test "7c229ea23205" {
  check 'x   :=   z     ;
GC :=   z *     y    ' '⇒ skip; GC := (z*y), {x → 0}
⇒ GC := (z*y), {x → 0}
⇒ skip, {GC → 0, x → 0}'
}
