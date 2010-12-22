.PHONY: all clean

PARAMS=-Psancta -XGtkAda_Include_Core=Yes -XHungarian_Include_Test=Yes -XHungarian_Include_Base=Yes -XHungarian_Link=Static_Library -XAgpl_Include_Boost=Yes -XAgpl_Include_Test=Yes -XAgpl_Include_Psql=Yes -XAgpl_Include_Http=No -XAgpl_Include_Gtk=Yes -XAgpl_Include_Concorde=Yes -XAgpl_Link=Static_Library -XPlayer2_Ada_Include_Test=Yes -XPlayer2_Ada_Link=Static_Library -XMbicp_Include_Test=Yes -XMbicp_Link=Static_Library -XSancta_Include_Gtk=Yes -XSancta_Include_Test=Yes -XSancta_Link=Static_Library -XSancta_Include_Slam=No

all:
	# make -C player-ada all
	# make -C agpl all
	# Not needed anymore, gprbuild takes care of it all
	#
	# NOTE: you don't need to build the library first. Just "with" the project file in your project and choose the appropriate linking type.
	#
	# gprmake builds everything
	#
	gprbuild ${PARAMS}

clean:
	rm -f obj/* libstatic/* libdynamic/*
	make -C agpl clean
	make -C player-ada clean
	make -C mbicp clean
	gprclean ${PARAMS}
