load ../../harness

@test "bded15cc5358" {
  check 'if ((x +y)     -  d2  =     1+   -4∨     y  -     y <    -2)      then x     :=z-     z else skip  ' '⇒ skip, {}'
}
