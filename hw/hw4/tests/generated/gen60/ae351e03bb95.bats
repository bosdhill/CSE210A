load ../../harness

@test "ae351e03bb95" {
  check 'a8 :=-1  *     y     ;z   :=y*   to ' '⇒ skip; z := (y*to), {a8 → 0}
⇒ z := (y*to), {a8 → 0}
⇒ skip, {a8 → 0, z → 0}'
}
