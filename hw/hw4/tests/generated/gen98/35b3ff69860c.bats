load ../../harness

@test "35b3ff69860c" {
  check 'z     :=     y   *     g;
pw  :=    x     ' '⇒ skip; pw := x, {z → 0}
⇒ pw := x, {z → 0}
⇒ skip, {pw → 0, z → 0}'
}
