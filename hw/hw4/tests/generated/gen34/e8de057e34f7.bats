load ../../harness

@test "e8de057e34f7" {
  check 'z   :=  A*    3;
   x :=   z     -   z ' '⇒ skip; x := (z-z), {z → 0}
⇒ x := (z-z), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
