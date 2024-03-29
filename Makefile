MAKEFLAGS = -s

all:
	cd src; make; cd ..
	cd examples; make; cd ..

clean:
	cd src; make clean
	cd examples; make clean
	
install: src/libscift.a
	echo ''
	echo -n 'Installing scift in ${HOME}/Software/scift ... '
	mkdir -p ${HOME}/Software/scift/finclude/
	cp src/*.mod ${HOME}/Software/scift/finclude/
	cp src/lib*.a ${HOME}/Software/scift/
	cp src/*.mod ${HOME}/Software/scift/finclude/
	cp -r utils ${HOME}/Software/scift/
	mkdir -p ${HOME}/Software/scift/bin
	find examples/ -executable -type f -exec cp {} ${HOME}/Software/scift/bin/ \;
	echo 'OK'
	echo ''
	echo '-----------------------------------------------------------------'
	echo ' Append the next lines into ~/.bashrc file'
	echo '-----------------------------------------------------------------'
	echo ''
	echo ' export PATH=$$PATH:$$HOME/Software/scift/bin'
	echo ' export PATH=$$PATH:$$HOME/Software/scift/utils'
	echo ' export LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$$HOME/Software/scift/'
	echo '-----------------------------------------------------------------'

uninstall: ${HOME}/Software/scift/libscift.a
	echo -n 'Uninstalling scift from ${HOME}/Software/scift ... '
	rm -rf ${HOME}/Software/scift
	echo 'OK'

doc: doxyfile doc/style.css src/libscift.a
	cp -r doc/images/ doc/html/
	doxygen doxyfile
