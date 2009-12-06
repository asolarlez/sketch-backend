# -*- coding: utf-8 -*-
# author: gatoatigrado (nicholas tung) [ntung at ntung]
# Copyright 2009 University of California, Berkeley

# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain a
# copy of the License at http://www.apache.org/licenses/LICENSE-2.0 .

Name: sketch-cegis
Summary: CEGIS backend for UC Berkeley's SKETCH
Version: 1.5.0SNAPSHOT
Release: 1
Url: http://sketch.cs.berkeley.edu/
Source0: sketch-cegis-1.5.0SNAPSHOT.tar.lzma
License: different modules under different licenses (BSD and LGPL)
Group: Development/Tools
BuildRoot: %{_tmppath}/%{name}-%{version}-build

# TODO -- delete me
Requires: java-1.6.0-openjdk
BuildRequires: lzma gcc gzip gcc-c++ binutils glibc-devel libstdc++-devel libstdc++ make bison flex
# for sketch-frontend
# Requires: java-1_6_0-openjdk
# BuildRequires: java-1_6_0-openjdk-devel

%description
SKETCH is a software synthesis tool that allows for rapid development of highly tuned bug-free algorithm implementations. To do this, the programmer develops a sketch, or partial implementation, and a separate specification of the desired functionality. The synthesizer then completes the sketch to behave like the specification. The correctness of the synthesized implementation is guaranteed by the compiler.

%doc README COPYING AUTHORS

%prep
%setup -q

%build
./autogen.sh
./configure --prefix=/usr
echo "using %{?_smp_mflags} processors for compilation"
make %{?jobs:-j%jobs}

%install
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(755, root, root)
/usr/bin/cegis
