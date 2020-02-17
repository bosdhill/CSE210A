load ../../harness

@test "488d05fb781b" {
  check 'z     :=  y   +   -4    ;

aq :=-3  *-2     ' '⇒ skip; aq := (-3*-2), {z → -4}
⇒ aq := (-3*-2), {z → -4}
⇒ skip, {aq → 6, z → -4}'
}
