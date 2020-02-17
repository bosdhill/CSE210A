load ../../harness

@test "9da330a91bf0" {
  check 'x :=y  *  -3;

qY     :=   1    -y' '⇒ skip; qY := (1-y), {x → 0}
⇒ qY := (1-y), {x → 0}
⇒ skip, {qY → 1, x → 0}'
}
