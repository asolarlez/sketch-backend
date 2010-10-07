#!/usr/bin/env python
# -*- coding: utf-8 -*-
# author: gatoatigrado (nicholas tung) [ntung at ntung]
# Copyright 2009 University of California, Berkeley

# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain a
# copy of the License at http://www.apache.org/licenses/LICENSE-2.0 .
from __future__ import division, print_function
from collections import namedtuple
from gatoatigrado_lib import (ExecuteIn, Path, SubProc, dict, get_singleton,
    list, memoize_file, pprint, process_jinja2, set, sort_asc, sort_desc)
from warnings import warn
import re, sys

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

def run_osc_commit(name, tmppath, commit_msg):
    repopath = Path("home:gatoatigrado1/%s" % (name))
    skip = False
    if Path("home:gatoatigrado1").exists():
        with ExecuteIn(Path("home:gatoatigrado1")):
            skip = list(SubProc(["osc", "status"]).exec_lines()) == []
    if not skip:
        try:
            SubProc(["osc", "co", "home:gatoatigrado1", name]).start_wait()
        except OSError:
            print("\n\n\nosc not found in path; install with 'sudo zypper -n in osc'", file=sys.stderr)
    else:
        print("skipping already checked out source.")
    [v.copy(repopath.subpath(v.basename())) for v in tmppath.files()]
    with ExecuteIn(repopath):
        SubProc(["osc", "add"] + [v for v in Path(".").listdir() if v != ".osc"]).start_wait()
        SubProc(["osc", "commit"] + (["-m", commit_msg]
            if commit_msg else [])).start_wait()

def run_jinja2(files, tmppath, name, version, sourcefile, release_number_v, pathmap):
    def output(fname, text):
        fname = fname.basename().replace(".jinja2", "")
        if fname.endswith("dsc"):
            fname = fname[:-4] + "-" + version + ".dsc"
        if fname in pathmap:
            pathmap[fname].write(text)
        else:
            tmppath.subpath(fname).write(text)
        print("\n\n\n%s\n>>> %s" %(fname, text.strip().replace("\n", "\n>>> ")))

    src_archive = tmppath.subpath(sourcefile)
    md5sum = src_archive.md5sum() if src_archive.exists() else "file doesn't exist"
    size = src_archive.getsize() if src_archive.exists() else "file doesn't exist"

    process_jinja2(glbls={
        "name": name,
        "version": version,
        "sourcefile": sourcefile,
        "md5sum": md5sum,
        "size": size,
        "release_number": release_number_v
        }, output_fcn=output).values()

def main(name, version, proj_path, conf_path, no_inc_release, tmpdir, run_local_install,
        osc_commit, commit_msg, upload_to_cobol, additional_path):

    proj_path, conf_path, tmpdir = Path(proj_path), Path(conf_path), Path(tmpdir)
    assert proj_path.subpath(".hg").isdir(), "please run in base directory (with .hg)"
    check_modified()

    version = get_sketch_version(proj_path) if version is None else version
    # NOTE -- need to use gz instead of lzma for Debian compatibility
    sourcefile = "%s_%s.orig.tar.gz" % (name, version)
    release_number_v = get_release_version(conf_path, no_inc_release)

    tmppath = tmpdir.subpath("%s-%s" % (name, version))
    if tmppath.exists():
        print("temporary directory exists; delete it? ", end="")
        sys.stdout.flush()
        if "y" in raw_input():
            tmppath.rmtree()
    tmppath.makedirs()

    srcpath = tmppath.subpath("%s-%s" % (name, version))
    proj_path.copytree(srcpath)

    pathmap = { "debian.control": srcpath.subpath("debian/control"),
        "debian.rules": srcpath.subpath("debian/rules"),
        "debian.changelog": srcpath.subpath("debian/changelog") }

    with ExecuteIn(srcpath):
        assert (Path(".") != proj_path)
        SubProc(["zsh", "-c", r"rm -rf $(hg stat -uin) .hg*"]).start_wait()

    files = [conf_path.subpath("%s.spec.jinja2" % (name))]
    nondsc_files = [v for v in files if not "dsc" in v]
    dsc_files = [v for v in files if "dsc" in v]
    with ExecuteIn(conf_path):
        run_jinja2(nondsc_files, tmppath, name, version,
            sourcefile, release_number_v, pathmap)

    if additional_path:
        additional_path = Path(additional_path)
        final_path = srcpath.subpath(additional_path.basename())
        final_path.rmtree()
        additional_path.copytree(final_path)

    with ExecuteIn(tmppath):
        SubProc(["tar", "cfa", sourcefile, srcpath.basename()]).start_wait()
        srcpath.rmtree()

    with ExecuteIn(conf_path):
        run_jinja2(dsc_files, tmppath, name, version,
            sourcefile, release_number_v, pathmap)

    with ExecuteIn(tmppath):
        if run_local_install:
            SubProc(["build"]).start_wait()
        if osc_commit:
            run_osc_commit(name, tmppath, commit_msg)

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
    cmdopts.add_option("--additional_path",
        help="copy an additional directory tree to the expanded files")

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
