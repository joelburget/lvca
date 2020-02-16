%%

reverse_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reverse_separated_nonempty_llist(separator, X); separator; x = X
    { x :: xs }

%inline reverse_separated_llist(separator, X):
    { [] }
| xs = reverse_separated_nonempty_llist(separator, X)
    { xs }

%public %inline separated_llist(separator, X):
  xs = reverse_separated_llist(separator, X)
    { List.rev xs }
