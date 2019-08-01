#!/usr/bin/env ocaml

#use "topfind"
#require "unix"

let testdir =
  if Array.length Sys.argv < 2 then
    Filename.concat
      (Filename.dirname Sys.argv.(0))
      "reftests"
  else Sys.argv.(1)

let test_files =
  Array.fold_right (fun f acc ->
      if Filename.extension f = ".test"
      then Filename.concat testdir f :: acc
      else acc)
    (Sys.readdir testdir)
    []

type test = {
  name: string;
  repo_hash: string;
  commands: (string * string list) list;
}

let cmd_prompt = "### "

let is_prefix pfx s =
  String.length s >= String.length pfx &&
  String.sub s 0 (String.length pfx) = pfx

let rem_prefix pfx s =
  if not (is_prefix pfx s) then invalid_arg "rem_prefix"
  else String.sub s (String.length pfx) (String.length s - String.length pfx)

(* Test file format: {v
   REPO_HASH
   ### opam command
   output line 1
   output...
   ### opam command
   output...
v}*)

let load_test f =
  let name = Filename.remove_extension f in
  let ic = open_in f in
  let repo_hash = try input_line ic with
    | End_of_file -> failwith "Malformed test file"
  in
  let commands =
    let rec aux commands =
      match input_line ic, commands with
      | s, commands when is_prefix cmd_prompt s ->
        aux ((rem_prefix cmd_prompt s, []) :: commands)
      | s, ((cmd,out) :: commands) ->
        aux ((cmd, s::out) :: commands)
      | exception End_of_file ->
        List.rev_map (fun (cmd, out) -> cmd, List.rev out) commands
      | _ -> failwith "Malformed test file"
    in
    aux []
  in
  close_in ic;
  { name; repo_hash; commands }

let command fmt =
  Printf.ksprintf (fun str ->
      let ret = Sys.command str in
      if ret <> 0 then
        Printf.ksprintf failwith
          "Command %s failed with code %d"
          str ret)
    fmt

let rec with_temp_dir f =
  let s =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "opam-reftest-%06x" (Random.int 0xffffff))
  in
  if Sys.file_exists s then
    with_temp_dir f
  else
  (command "mkdir -p %s" s;
   let r = f s in
   command "rm -rf %s" s;
   r)

let with_repo hash f =
  with_temp_dir (fun repo_dir ->
      let tgz = Filename.concat repo_dir "archive.tar.gz" in
      command "wget -O %s https://github.com/ocaml/opam-repository/archive/%s.tar.gz"
        tgz hash;
      command "cd %s && tar xzf --strip-components=1 %s"
        repo_dir tgz;
      command "rm -f %s" tgz)

let opam_cmd =
  let ( / ) = Filename.concat in
  testdir / ".." / ".." / "opam"

let run_cmd opamroot logfile fmt =
  Printf.ksprintf (fun cmd ->
      match String.split_on_char ' ' cmd with
      | "opam" :: cmd :: args ->
        command "%s %s --root=%s %s >>%s 2>&1"
          opam_cmd cmd opamroot (String.concat " " args)
          logfile
      | _ ->
        command "%s >>%s 2>&1" cmd logfile)
    fmt

let run_test t =
  with_temp_dir @@ fun opamroot ->
  with_repo t.repo_hash @@
  command "opam init --root=%s --no-setup --bare file://%s" opamroot;
  let logfile = Filename.concat (testdir^".out") t.name in
  close_out (open_out logfile);
  List.iter (fun (cmd, _) ->
      run_cmd opamroot logfile "echo '### '%s" (Filename.quote cmd);
      run_cmd opamroot logfile "%s" cmd)
    t.commands

let () =
  List.iter (fun f ->
      Printf.eprintf "Testing %s...\n" f;
      let t = load_test f in
      run_test t)
    test_files
