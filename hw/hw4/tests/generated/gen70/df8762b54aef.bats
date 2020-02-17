load ../../harness

@test "df8762b54aef" {
  check 'z:= z+    z  ' '⇒ skip, {z → 0}'
}
