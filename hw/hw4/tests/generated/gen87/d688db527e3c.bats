load ../../harness

@test "d688db527e3c" {
  check 'z   :=     -1 *     Z  ;k2 :=    2   -  -1 ' '⇒ skip; k2 := (2--1), {z → 0}
⇒ k2 := (2--1), {z → 0}
⇒ skip, {k2 → 3, z → 0}'
}
