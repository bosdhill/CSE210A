load ../../harness

@test "3d291bbfc422" {
  check 'VG  := -2  + 2     ;Ko  :=    4   *1  ' '⇒ skip; Ko := (4*1), {VG → 0}
⇒ Ko := (4*1), {VG → 0}
⇒ skip, {Ko → 4, VG → 0}'
}
