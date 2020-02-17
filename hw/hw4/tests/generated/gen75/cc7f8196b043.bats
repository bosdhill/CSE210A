load ../../harness

@test "cc7f8196b043" {
  check 'if (¬(y + 3=     z *  wf))     then     z    :=     -2     -   x   else y   :=y     *    y   ' '⇒ z := (-2-x), {}
⇒ skip, {z → -2}'
}
