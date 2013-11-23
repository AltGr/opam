(**************************************************************************)
(*                                                                        *)
(*    Copyright 2013 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open Cmdliner

type options = {
  file: OpamFilename.t;
  pkgname: OpamPackage.Name.t;
  prefix: OpamFilename.Dir.t;
  script: bool;
}

type commands = {
  mkdir: OpamFilename.Dir.t -> unit;
  rmdir: OpamFilename.Dir.t -> unit;
  cp: src:OpamFilename.t -> dst:OpamFilename.t -> unit;
  rm: OpamFilename.t -> unit;
}

let do_commands = {
  mkdir = OpamFilename.mkdir;
  rmdir = OpamFilename.rmdir;
  cp = OpamFilename.copy;
  rm = OpamFilename.remove;
}

let script_commands project_root ochan =
  let made_dirs = ref [] in
  Printf.fprintf ochan "!#/bin/bash\n";
  let mkdir dir =
    if not (List.mem dir !made_dirs) then (
      Printf.fprintf ochan "mkdir -p %S\n" (OpamFilename.Dir.to_string dir);
      made_dirs := dir :: !made_dirs
    ) in
  let rmdir dir =
    Printf.fprintf ochan "rmdir %S\n" (OpamFilename.Dir.to_string dir)
  in
  let cp ~src ~dst =
    mkdir (OpamFilename.dirname dst);
    (* use 'install' to set permissions ? *)
    Printf.fprintf ochan "cp -f %S %S\n"
      (OpamFilename.remove_prefix project_root src)
      (OpamFilename.to_string dst)
  in
  let rm file =
    Printf.fprintf ochan "rm %S\n" (OpamFilename.to_string file)
  in
  { mkdir; rmdir; cp; rm }


let install options =
  let instfile = OpamFile.Dot_install.safe_read options.file in
  let project_root = OpamFilename.cwd () in
  let cmd =
    if options.script then script_commands project_root stdout
    else do_commands
  in
  let warnings = ref [] in
  let check ~src ~dst base =
    let src_file = OpamFilename.create src base.c in
    if not (OpamFilename.exists src_file) then
      if base.optional then (* XXX if options.script ? *)
        (if not options.script then
           OpamGlobals.msg "%s <skipped>"
             (OpamFilename.to_string src_file))
      else
        warnings := (dst, base.c) :: !warnings;
    OpamFilename.exists src_file
  in
  let install_files (dst_dir, files) =
    if files <> [] && not (OpamFilename.exists_dir dst_dir) then (
      if not options.script then
        OpamGlobals.msg "creating %s\n%!" (OpamFilename.Dir.to_string dst_dir);
      cmd.mkdir dst_dir;
    );
    List.iter (fun (base, dst) ->
        let src_file = OpamFilename.create project_root base.c in
        let dst_file = match dst with
          | None   -> OpamFilename.create dst_dir (OpamFilename.basename src_file)
          | Some d -> OpamFilename.create dst_dir d in
        if check ~src:project_root ~dst:dst_dir base then (
          if not options.script then
            OpamGlobals.msg "%16s => %s\n" (OpamFilename.Base.to_string base.c)
              (OpamFilename.to_string dst_file);
          cmd.cp ~src:src_file ~dst:dst_file;
        )
      ) files
  in
  List.iter install_files
    (let module D = OpamPath.Switch in
     let module S = OpamFile.Dot_install in
     let instdir f = f options.prefix (OpamSwitch.of_string "") in
     let instf f = f instfile in
     [ instdir D.bin, instf S.bin;
       instdir D.sbin, instf S.sbin;
       instdir D.lib options.pkgname, instf S.sbin;
       instdir D.toplevel, instf S.toplevel;
       instdir D.stublibs, instf S.stublibs;
       instdir D.man_dir, instf S.man;
       instdir D.share options.pkgname, instf S.share;
       instdir D.etc options.pkgname, instf S.share;
       instdir D.doc options.pkgname, instf S.doc; ]);
  List.iter
    (fun (src, dst) -> (* XXX questions in script *)
       let src_file = OpamFilename.create (OpamFilename.cwd ()) src.c in
       if OpamFilename.exists dst then
         (if OpamState.confirm "%s exists. Overwrite ?" (OpamFilename.to_string dst)
          then cmd.cp ~src:src_file ~dst)
       else if
         OpamState.confirm "Do you want to install %s to %s.\n"
           (OpamFilename.Base.to_string src.c) (OpamFilename.to_string dst);
       then cmd.cp ~src:src_file ~dst
    ) (OpamFile.Dot_install.misc instfile)


let uninstall options =
  Printf.eprintf "UNINSTALL %s => %s (%b)\n%!"
    (OpamFilename.to_string options.file)
    (OpamFilename.Dir.to_string options.prefix)
    options.script

let options =
  let file =
    let doc = "The OPAM .install file to read for installation instructions" in
    Arg.(required & pos 0 (some string) None & info ~docv:"PKG.install" ~doc [])
  in
  let prefix =
    let doc = "The prefix to install to (default \"/usr/local\")" in
    Arg.(value & opt string "/usr/local" &  info ~docv:"PREFIX" ~doc ["prefix"])
  in
  let script =
    let doc = "Don't execute the commands, but output a shell-script" in
    Arg.(value & flag & info ~doc ["script"])
  in
  let pkgname =
    let doc = "Specify the package name. Used to set install directory under `share/', etc. \
               By default, basename of the .install file" in
    Arg.(value & opt (some string) None & info ~docv:"NAME" ~doc ["name"])
  in
  let make_options file prefix script name =
    let file =
      let f = OpamFilename.of_string (file ^ ".install") in
      if OpamFilename.exists f then f else
        let f = OpamFilename.of_string file in
        if OpamFilename.exists f then f else
          failwith ("File not found: " ^ file)
    in
    let prefix = OpamFilename.Dir.of_string prefix in
    let pkgname = match name with
      | Some n -> OpamPackage.Name.of_string n
      | None ->
        OpamPackage.Name.of_string
          (OpamFilename.remove_suffix (OpamFilename.Base.of_string ".install") file)
    in
    { file; prefix; script; pkgname }
  in
  Term.(pure make_options $ file $ prefix $ script $ pkgname)

let default_cmd =
  let doc = "Installs package files following instructions from an OPAM *.install file." in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "opam-install" ~version:OpamVersion.(to_string current) ~doc

let install_cmd =
  let doc =
    "Installs package files following instructions from an OPAM *.install file."
  in
  Term.(pure install $ options),
  Term.info "install" ~version:OpamVersion.(to_string current) ~doc

let uninstall_cmd =
  let doc = "Remove the package." in
  Term.(pure uninstall $ options),
  Term.info "uninstall" ~doc

let () =
  try
    match
      Term.eval_choice ~catch:false default_cmd [install_cmd; uninstall_cmd]
    with
    | `Error _ -> exit 2
    | _ -> exit 0
  with
  | OpamGlobals.Exit i -> exit i
