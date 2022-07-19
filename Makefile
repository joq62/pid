all:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf rebar.lock;
	rm -rf ebin;
	mkdir test_ebin;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
check:
	rebar3 check

eunit:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf rebar.lock;
	rm -rf ebin;
	mkdir test_ebin;
	mkdir ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa ebin -pa test_ebin -sname pid_test -run basic_eunit start -setcookie solis_cookie
