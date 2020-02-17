load ../../harness

@test "6bf6d4341b15" {
  check 'x     :=4    *   z;zE:=-1' '⇒ skip; zE := -1, {x → 0}
⇒ zE := -1, {x → 0}
⇒ skip, {x → 0, zE → -1}'
}
