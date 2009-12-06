#!/usr/bin/env python
# -*- coding: utf-8 -*-
# author: gatoatigrado (nicholas tung) [ntung at ntung]
# Copyright 2009 University of California, Berkeley

# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain a
# copy of the License at http://www.apache.org/licenses/LICENSE-2.0 .
from __future__ import division, print_function
from collections import namedtuple
from warnings import warn

try:
    from gatoatigrado_lib import (ExecuteIn, Path, SubProc, dict, get_singleton,
        list, memoize_file, pprint, process_jinja2, set, sort)
except:
    raise ImportError("please install gatoatigrado's utility library from "
            "bitbucket.org/gatoatigrado/gatoatigrado_lib")

modpath = Path(__file__).parent()
release_number = modpath.subpath("release_number")
if not release_number.isfile():
    release_number.write("0\n")

def main(name, version, no_inc_release, tmpdir, run_local_install, osc_commit, commit_msg, upload_to_cobol):
    original_path = Path(".")
    assert Path(".hg").isdir(), "please run in base sketch-backend directory"
    if version is None:
        mvnfile = Path("../sketch-frontend/pom.xml")
        assert mvnfile.isfile(), "please either provide a version or " \
            "check out sketch-frontend in ../sketch-frontend."
        from amara import bindery
        version = str(bindery.parse(mvnfile.read()).project.version)
    version = version.replace("-", "").replace(" ", "").replace("_", "")

    sourcefile = "%s-%s.tar.lzma" %(name, version)

    if "+" in "".join(SubProc(["hg", "identify"]).exec_lines()):
        warn("modified files in local directory; copying modified "
            "versions (but not new untracked files)")

    release_number_v = int(release_number.read().strip())
    if not no_inc_release:
        release_number_v += 1
        release_number.write(str(release_number_v) + "\n")

    process_jinja2(files=[modpath.subpath("%s.spec.jinja2" %(name))], glbls={
        "version": version,
        "sourcefile": sourcefile,
        "release_number": release_number_v
        })

    specpath = modpath.subpath("%s.spec" %(name))
    print(specpath.read(), end="")

    tmppath = Path(tmpdir).subpath("%s-%s" %(name, version))
    tmppath.makenewdir()

    specpath.copyinto(tmppath)
    srcpath = tmppath.subpath("%s-%s" %(name, version))
    Path(".").copytree(srcpath)

    with ExecuteIn(srcpath):
        assert (Path(".") != original_path)
        SubProc(["zsh", "-c", r"rm -rf $(hg stat -uin) .hg*"]).start_wait()

    with ExecuteIn(tmppath):
        SubProc(["tar", "cfa", sourcefile, srcpath.basename()]).start_wait()
        srcpath.rmtree()
        if run_local_install:
            SubProc(["rpm", "-iv", "--specfile", specpath.basename()]).start_wait()
        if osc_commit:
            SubProc(["osc", "co", "home:gatoatigrado1", "sketch-cegis"]).start_wait()
            repopath = Path("home:gatoatigrado1/sketch-cegis")
            [v.copy(repopath.subpath(v.basename())) for v in tmppath.files()]
            with ExecuteIn(repopath):
                SubProc(["osc", "commit"] + (["-m", commit_msg]
                    if commit_msg else [])).start_wait()

    if upload_to_cobol:
        with ExecuteIn(Path(tmpdir)):
            repopath.parent().rmtree()
            tar_name = sourcefile.replace("lzma", "bz2")
            SubProc(["tar", "cfa", tar_name, tmppath.basename()]).start_wait()
            SubProc(["scp", tar_name, "ntung@cobol.cs.berkeley.edu:public_html/temp"])

if __name__ == "__main__":
    import optparse
    cmdopts = optparse.OptionParser(usage="%prog [options]")
    cmdopts.add_option("--name", default="sketch-cegis",
        help="name (almost always don't set, for default sketch-cegis)")
    cmdopts.add_option("--version", help="set the version number of the build")
    cmdopts.add_option("--tmpdir", default="/tmp",
        help="temporary directory to copy files for the rpm")
    cmdopts.add_option("--commit_msg", help="commit message for osc")
    cmdopts.add_option("--no_inc_release", action="store_true",
        help="don't increment the release number in 'distconfig/linux-rpm/release_number'")
    cmdopts.add_option("--run_local_install", action="store_true",
        help="run rpm -vba on the spec file")
    cmdopts.add_option("--osc_commit", action="store_true",
        help="commit to the opensuse build service")
    cmdopts.add_option("--upload_to_cobol", action="store_true",
        help="upload an archive of the files to cobol")
    options, args = cmdopts.parse_args()
    main(*args, **options.__dict__)
