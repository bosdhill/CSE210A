load ../../harness

@test "3bc8116774f6" {
  check 'Cl    := 4 -     x;
cv:=x    ' '⇒ skip; cv := x, {Cl → 4}
⇒ cv := x, {Cl → 4}
⇒ skip, {Cl → 4, cv → 0}'
}
