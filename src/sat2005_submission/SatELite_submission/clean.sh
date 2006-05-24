export FM=$PWD/ForMani
cd SatELite
make realclean
cd ..
rm -f SatELite_release

cd MiniSatnik
make clean
cd ..
rm -f minisat_static 

rm -f ForMani/ADTs/depend.mak
rm -f ForMani/Global/depend.mak
