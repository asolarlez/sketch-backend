export FM=$PWD/ForMani

cd SatELite
make realclean
make r
cp SatELite_release ..
cd ..

cd MiniSatnik
make clean
make rs
cp minisat_static ..
cd ..

echo
echo 'BUILD DONE!'
echo
echo 'Put "SatELite_release", "minisat_static", and "SatELiteGTI" wherever you'
echo 'want to, then run "SatELiteGTI" from that place for happy SAT-solving.'
