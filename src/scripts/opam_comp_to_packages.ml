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

type args = OpamFilename.Dir.t

let args =
  let open Cmdliner in
  let dir =
    let doc = "Root directory of the repo to process (default cwd)" in
    Arg.(value & opt (some dir) None & info ["repo"] ~doc)
  in
  Term.(pure (function None -> OpamFilename.cwd ()
                     | Some d -> OpamFilename.Dir.of_string d)
        $ dir)

let comp_to_package repo c prefix =
  let comp_f = OpamPath.Repository.compiler_comp repo prefix c in
  let descr_f = OpamPath.Repository.compiler_descr repo prefix c in
  let comp = OpamFile.Comp.read comp_f in
  let descr = OpamFile.Descr.read descr_f in
  let version =
    OpamPackage.Version.of_string
      (OpamCompiler.to_string (OpamFile.Comp.name comp))
  in
  let nv = OpamPackage.create (OpamPackage.Name.of_string "ocaml") version in
  OpamGlobals.msg "Processing compiler %s => package %s\n"
    (OpamCompiler.to_string (OpamFile.Comp.name comp))
    (OpamPackage.to_string nv);
  let nofilter x = x, (None: filter option) in
  let build =
    OpamFile.Comp.(
      match build comp with
      | [] ->
        List.map (fun l -> nofilter (List.map nofilter l)) [
          (List.map (fun s -> CString s) ("./configure" :: configure comp ))
          @ [ CString "-prefix"; CIdent "prefix"];
          CIdent "make" :: List.map (fun s -> CString s) (make comp);
          [ CIdent "make"; CString "install"];
        ]
      | cl -> cl)
  in
  let prefix = Some (OpamPackage.Name.to_string (OpamPackage.name nv)) in
  let patches =
    List.map (fun f ->
        OpamFilename.download ~overwrite:true f
          (OpamPath.Repository.files repo prefix nv)
        |> OpamFilename.basename
      )
      (OpamFile.Comp.patches comp)
  in
  let (@) f x y = f y x in
  let opam =
    let module O = OpamFile.OPAM in
    O.create nv
    |> O.with_build @ build
    |> O.with_maintainer @ [ "contact@ocamlpro.com" ]
    |> O.with_patches @ List.map nofilter patches
  in
  (match OpamFile.Comp.src comp with
   | None -> ()
   | Some address ->
     let url = OpamFile.URL.create None address in
     OpamFile.URL.write (OpamPath.Repository.url repo prefix nv) url);
  OpamFile.OPAM.write (OpamPath.Repository.opam repo prefix nv) opam;
  OpamFile.Descr.write (OpamPath.Repository.descr repo prefix nv) descr;
  let comp =
    let module C = OpamFile.Comp in
    comp
    |> C.with_src @ None
    |> C.with_patches @ []
    |> C.with_configure @ []
    |> C.with_make @ []
    |> C.with_build @ []
    |> C.with_packages @
       OpamFormula.(
         And (Atom (OpamPackage.name nv, Atom (`Eq, OpamPackage.version nv)),
              C.packages comp)
       )
  in
  OpamFile.Comp.write comp_f comp

let rewrite_ocaml_deps repo nv prefix =
  let opam_f = OpamPath.Repository.opam repo prefix nv in
  let opam = OpamFile.OPAM.read opam_f in
  let ocaml_version = OpamFile.OPAM.ocaml_version opam in
  match ocaml_version with
  | None -> ()
  | Some v ->
    OpamGlobals.msg "Rewriting ocaml version constraint in package %s\n"
      (OpamPackage.to_string nv);
    let depends = OpamFormula.(
        And (Atom (OpamPackage.Name.of_string "ocaml",
                   map (fun (op,v) ->
                       Atom (op, OpamPackage.Version.of_string (OpamCompiler.Version.to_string v))
                     ) v),
             OpamFile.OPAM.depends opam)
      )
    in
    let opam = OpamFile.OPAM.with_ocaml_version opam None in
    let opam = OpamFile.OPAM.with_depends opam depends in
    OpamFile.OPAM.write opam_f opam
(* Warning : no conversion done on ocaml-version in filters (but there are very few.
   Sed s/ocaml-version/ocaml:version/g is your friend, really *)

let process repo_root =
  let repo = OpamRepository.local repo_root in
  let compilers = OpamRepository.compilers_with_prefixes repo in
  let packages = OpamRepository.packages_with_prefixes repo in
  OpamGlobals.header_msg "Converting compiler definitions";
  OpamCompiler.Map.iter (comp_to_package repo) compilers
  ;
  OpamGlobals.header_msg "Converting package constraints";
  OpamPackage.Map.iter (rewrite_ocaml_deps repo) packages
