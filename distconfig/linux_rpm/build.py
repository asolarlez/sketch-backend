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
import re

try:
    from gatoatigrado_lib import (ExecuteIn, Path, SubProc, dict, get_singleton,
        list, memoize_file, pprint, process_jinja2, set, sort)
except:
    raise ImportError("please install gatoatigrado's utility library from "
            "bitbucket.org/gatoatigrado/gatoatigrado_lib")

# TODO -- delete me
# Requires: java-1.6.0-openjdk
# BuildRequires: lzma gcc gzip gcc-c++ binutils glibc-devel libstdc++-devel libstdc++ make bison flex
# for sketch-frontend
# Requires: java-1_6_0-openjdk
# BuildRequires: java-1_6_0-openjdk-devel

def get_release_version(conf_path, no_inc_release):
    release_number = conf_path.subpath("release_number")
    release_number_v = int(release_number.read().strip())
    if not release_number.isfile():
        release_number.write("0\n")
    if not no_inc_release:
        release_number_v += 1
        release_number.write(str(release_number_v) + "\n")
    return release_number_v

def get_sketch_version(proj_path):
    return get_singleton(proj_path.subpath("configure.ac").grep(
        re.compile("AC_INIT\(\w+, ([0-9\.]+),"))).group(1)

def check_modified():
    if "+" in "".join(SubProc(["hg", "identify"]).exec_lines()):
        warn("modified files in local directory; copying modified "
            "versions (but not new untracked files)")

def main(name, version, proj_path, conf_path, no_inc_release, tmpdir, run_local_install,
        osc_commit, commit_msg, upload_to_cobol):

    proj_path, conf_path, tmpdir = Path(proj_path), Path(conf_path), Path(tmpdir)
    assert proj_path.subpath(".hg").isdir(), "please run in base sketch-backend directory"
    check_modified()

    version = get_sketch_version(proj_path) if version is None else version
    sourcefile = "%s-%s.tar.lzma" % (name, version)
    release_number_v = get_release_version(conf_path, no_inc_release)

    spec = get_singleton(process_jinja2(files=[conf_path.subpath("%s.spec.jinja2" % (name))], glbls={
        "version": version,
        "sourcefile": sourcefile,
        "release_number": release_number_v
        }, output_fcn=None).values())
    print(spec)

    tmppath = tmpdir.subpath("%s-%s" % (name, version))
    tmppath.makenewdir()

    tmppath.subpath("%s.spec" % (name)).write(spec)
    srcpath = tmppath.subpath("%s-%s" % (name, version))
    proj_path.copytree(srcpath)

    with ExecuteIn(srcpath):
        assert (Path(".") != proj_path)
        SubProc(["zsh", "-c", r"rm -rf $(hg stat -uin) .hg*"]).start_wait()

    with ExecuteIn(tmppath):
        SubProc(["tar", "cfa", sourcefile, srcpath.basename()]).start_wait()
        srcpath.rmtree()
        if run_local_install:
            SubProc(["build"]).start_wait()
        repopath = Path("home:gatoatigrado1/%s" % (name))
        if osc_commit:
            SubProc(["osc", "co", "home:gatoatigrado1", name]).start_wait()
            [v.copy(repopath.subpath(v.basename())) for v in tmppath.files()]
            with ExecuteIn(repopath):
                SubProc(["osc", "commit"] + (["-m", commit_msg]
                    if commit_msg else [])).start_wait()

    if upload_to_cobol:
        with ExecuteIn(tmpdir):
            if osc_commit:
                repopath.parent().rmtree()
            tar_name = sourcefile.replace("lzma", "tar")
            SubProc(["tar", "cfa", tar_name, tmppath.basename()]).start_wait()
            SubProc(["scp", tar_name, "ntung@cobol.cs.berkeley.edu:public_html/temp"])

def add_generic_opts(cmdline):
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

if __name__ == "__main__":
    import optparse
    cmdopts = optparse.OptionParser(usage="%prog [options]")
    cmdopts.add_option("--name", default="sketch-cegis",
        help="name (almost always don't set, for default sketch-cegis)")
    cmdopts.add_option("--version", help="set the version number of the build")
    cmdopts.add_option("--proj_path", default=".", help="path of the project to upload")
    cmdopts.add_option("--conf_path", default="distconfig/linux_rpm",
        help="path of configuration files")
    add_generic_opts(cmdopts)
    options, args = cmdopts.parse_args()
    main(*args, **options.__dict__)
