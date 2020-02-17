load ../../harness

@test "cd76f07d2847" {
  check 'z   :=   1     *    z    ;
z   := 4   *  x   ' '⇒ skip; z := (4*x), {z → 0}
⇒ z := (4*x), {z → 0}
⇒ skip, {z → 0}'
}
