#cd $SKETCH/sketch-backend
#./configure
#make -j2
#sudo make install
cd $SKETCH/sketch-frontend
make run-local-seq EXEC_ARGS="src/test/sk/seq/miniTest1.sk"
make system-install DESTDIR=/usr/bin SUDOINSTALL=1
