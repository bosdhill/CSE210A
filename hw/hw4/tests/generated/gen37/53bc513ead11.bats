load ../../harness

@test "53bc513ead11" {
  check 'z   := z+     -3; i := -2 +     1 ' '⇒ skip; i := (-2+1), {z → -3}
⇒ i := (-2+1), {z → -3}
⇒ skip, {i → -1, z → -3}'
}
