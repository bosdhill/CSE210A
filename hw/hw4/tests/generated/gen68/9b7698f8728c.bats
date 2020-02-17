load ../../harness

@test "9b7698f8728c" {
  check 'Hd :=4   *  kx   ;
UT  := -4  -z' '⇒ skip; UT := (-4-z), {Hd → 0}
⇒ UT := (-4-z), {Hd → 0}
⇒ skip, {Hd → 0, UT → -4}'
}
