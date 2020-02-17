load ../../harness

@test "f2d5cf544dcb" {
  check 'if (¬(3<     2+    x))    then skip    else  Ga:=    -2     *     -4    ' '⇒ skip, {}'
}
