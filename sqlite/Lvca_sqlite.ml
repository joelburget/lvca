open Sqlite3
open Lvca_syntax

type hash = string
type tag = string
type content = Nominal.Term.t
type return_code = Sqlite3.Rc.t

let check_rc rc = match rc with Rc.OK -> None | r -> Some r

module Db = struct
  type t = db

  let open_ db_name = db_open db_name

  let create_tables_sql =
    {|
  CREATE TABLE tags (
    tag TEXT PRIMARY KEY,
    hash TEXT NOT NULL UNIQUE
  );

  CREATE TABLE hashes (
    hash TEXT PRIMARY KEY,
    content TEXT NOT NULL
  );
  |}
  ;;

  let create_tables db = exec db create_tables_sql
  let create_tables' db = create_tables db |> check_rc
end

module Hash = struct
  type t = hash

  let lookup db hash =
    let query_sql = Printf.sprintf "SELECT content FROM hashes WHERE hash=%S" hash in
    let result = ref None in
    let cb row = result := Some row.(0) in
    match exec_not_null_no_headers db ~cb query_sql with
    | Rc.OK ->
      let result =
        match !result with
        | None -> None
        | Some str -> str |> Bytes.of_string |> Nominal.Term.deserialize
      in
      Ok result
    | rc -> Error rc
  ;;

  let insert db tm =
    let hash = Nominal.Term.hash tm in
    let content = Nominal.Term.serialize tm |> Bytes.to_string in
    let stmt =
      (* TODO: use single quotes *)
      Printf.sprintf "INSERT INTO content ('hash', 'content') VALUES(%S, %S)" hash content
    in
    exec db stmt
  ;;
end

module Tag = struct
  type t = tag

  let dereference_tag db tag =
    let query_sql = Printf.sprintf "SELECT hash FROM tags WHERE tag=%S" tag in
    let result = ref None in
    let cb row = result := Some row.(0) in
    match exec_not_null_no_headers db ~cb query_sql with
    | Rc.OK -> Ok !result
    | rc -> Error rc
  ;;

  let lookup_tag db tag =
    match dereference_tag db tag with
    | Ok (Some hash) -> Hash.lookup db hash
    | Ok None -> Ok None
    | Error err -> Error err
  ;;

  let set db hash tag =
    let stmt =
      (* TODO: use single quotes *)
      Printf.sprintf "INSERT OR REPLACE INTO tags ('tag', 'hash') VALUES(%S, %S)" tag hash
    in
    exec db stmt
  ;;
end
