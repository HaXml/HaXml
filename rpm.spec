%define ghc_version 6.0

Summary: Haskell utilities for processing XML
Name: HaXml
Version: 1.09
Release: 3
License: Artistic
Group: Development/Languages/Haskell
URL: http://www.haskell.org/HaXml/
Source0: %{name}-%{version}.tar.gz
Patch1: HaXml-1.09-ghc-mk-pkg.patch
NoSource: 0
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot

%description
HaXml is a collection of utilities for using Haskell and XML together.
Its basic facilities include:

    * a parser for XML,
    * a separate error-correcting parser for HTML,
    * an XML validator,
    * pretty-printers for XML and HTML, 
    * a combinator library for XML transformation,
    * a translator from DTD to Haskell datatypes.


%package ghc%{ghc_version}
Summary: Haskell library for processing XML
Group: Development/Languages/Haskell
Requires: ghc = %{ghc_version}
BuildRequires: ghc = %{ghc_version}

%description ghc%{ghc_version}
HaXml is a collection of utilities for using Haskell and XML together.

This package contains the library built for ghc-%{ghc-version}.

%package doc
Summary: Haskell utilities for processing XML documentation
Group: Development/Languages/Haskell

%description doc
HaXml is a collection of utilities for using Haskell and XML together.

This package contains the documentation.

%define ghclibdir %{_libdir}/ghc-%{ghc_version}

%prep
%setup -q
%patch1 -p1 -b .pkg

%build
./configure --buildwith=ghc-%{ghc_version} --prefix=%{buildroot}%{_bindir}
make

%install
rm -rf $RPM_BUILD_ROOT
perl -pi -e "s|^%{_prefix}|%{buildroot}%{_prefix}|"
obj/ghc/{ghcincdir,ghclibdir}
mkdirhier %{buildroot}%{_bindir}
mkdirhier %{buildroot}%{ghclibdir}/imports/HaXml
%makeinstall
cp -p obj/ghc/pkg.conf %{buildroot}%{ghclibdir}/imports/HaXml/package.conf

%clean
rm -rf $RPM_BUILD_ROOT

%post ghc%{ghc_version}
ghc-pkg-%{ghc_version} --update-package <
%{ghclibdir}/imports/HaXml/package.conf

%preun ghc%{ghc_version}
ghc-pkg-%{ghc_version} --remove-package=%{name}

%files
%defattr(-,root,root,-)
%{_bindir}/*
%doc COPYRIGHT LICENSE README

%files ghc%{ghc_version}
%defattr(-,root,root,-)
%{ghclibdir}
%doc COPYRIGHT LICENSE README examples

%files doc
%defattr(-,root,root,-)
%doc docs

%changelog
* Tue Jun 10 2003 Jens Petersen <petersen@haskell.org> - 1.09-3
- fix post script package location and preun script package name
- no need to make the ghci object file

* Tue Jun 10 2003 Jens Petersen <petersen@haskell.org> - 1.09-2
- add doc files
- include examples in lib package
- add doc package

* Tue Jun 10 2003 Jens Petersen <petersen@haskell.org> - 1.09-1
- Initial packaging.

