## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s infra_master_lib start -sname infra_master 

build:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump;
	erlc -o ebin src/*.erl ../../common/dets_dbase/src/*erl;
	cp src/*.app ebin
create:
	erlc -o ../ebin_working_nodes src/*.erl;
	cp src/*.app ../ebin_working_nodes
clean:
	rm -rf */*~ *.beam ebin/*.beam src/*.beam test_src/*.beam */*~ *~ *.dump
start:
	erl -pa ebin -pa ../dev/sd/ebin -run infra_master start_infra -sname infra_master
tar:
	tar -cvf infra_master.tar *

test:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump */*.beam;
	erlc -o ebin src/*.erl test_src/*.erl ../common/dets_dbase/src/*erl ../template/src/*.erl;
	cp src/*.app ebin;
	cp ../template/src/*.app ebin;
	erl -pa ebin -pa ../repo_mgr/ebin -s test_infra_master start -sname infra_master
