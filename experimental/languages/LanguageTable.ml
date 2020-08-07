let abstract_syntax =
  {|
import {list, string} from "lvca/builtin";

rowType := rowType(list(atomicType()))

atomicType := string() ???
atomicValue := string(string) ???

row := row(list(atomicValue))

table := table(rowType; list(row))
|}
;;
