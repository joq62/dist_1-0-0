## set the paths for a default setup

all:
	erl -pa ebin -s infra_dbase start_dbase -sname infra_dbase
#	cp ebin/* ../ebin;
#	erl -pa ebin -pa /home/joq/erlang/lib/ebin -s dbase start

clean:
	rm -rf ebin/*.beam */*~ src/*.beam *~ *.dump

test:
	rm -rf ebin/* */*~ src/*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erl -pa ebin -s test_dets_dbase test -sname test_dets_dbase
