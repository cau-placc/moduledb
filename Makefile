# Generic Makefile for Spicey applications

# The compilation target (IFI or IFIPAKCS for real deployment, TEST for mdbtest)
TARGET = TEST

# check setting of TARGET variable:
ifeq ($(TARGET),IFI)
# directory with all data:
DATADIR=/srv/sites/ps.informatik.uni-kiel.de/mdb/mdb
# Definition of the Curry installation bin directory to be used:
CURRYHOME=/opt/kics2/kics2
else ifeq ($(TARGET),IFIPAKCS)
# directory with all data:
DATADIR=/srv/sites/ps.informatik.uni-kiel.de/mdb/mdb
# Definition of the Curry installation bin directory to be used:
CURRYHOME=/opt/pakcs/pakcs
else ifeq ($(TARGET),TEST)
# directory with all data:
DATADIR=$(HOME)/home/data/mdbtest
# Definition of the Curry installation directory to be used:
#CURRYHOME=/opt/kics2/kics2
CURRYHOME=$(HOME)/pakcs
else
error:
	echo "ERROR: invalid definition of variable TARGET!"
	echo "Please use 'TARGET=IFI', 'TARGET=IFIPAKCS' or 'TARGET=TEST'
	exit 1
endif

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored, e.g.: $(HOME)/public_html
WEBSERVERDIR=$(HOME)/public_html/mdbtest

# Name of the compile cgi program
ifeq ($(TARGET),IFI)
CGIPROGRAM=$(WEBSERVERDIR)/show.cgi
else ifeq ($(TARGET),IFIPAKCS)
WEBSERVERDIR=$(HOME)/public_html/mdbtest/pakcs
CGIPROGRAM=$(WEBSERVERDIR)/show.cgi
else
CGIPROGRAM=$(WEBSERVERDIR)/mdb.cgi
endif

# Curry bin directory to be used:
export CURRYBIN=$(CURRYHOME)/bin

# Default options for compiling Curry programs
CURRYOPTIONS=:set -time

# Executable of the Curry Package Manager CPM:
CPM := $(CURRYBIN)/cypm

# directory with all session data:
SESSIONDATADIR=$(DATADIR)/sessiondata

# The root directory of the package of the Spicey application:
PKGDIR := $(CURDIR)

# The root directory of the model sources of the Spicey application:
MODELDIR := $(PKGDIR)/src/Model

# Executable of the makecgi:
CURRY2CGI := $(shell which curry2cgi)

.PHONY: all
all:
	@echo "make: deploy install compile load run clean?"

# Install the packages required by the generated Spicey application:
.PHONY: install
install:
	$(CPM) $(CPMOPTIONS) update
	$(CPM) $(CPMOPTIONS) install

# check presence of tools required for deployment:
.PHONY: checkdeploy
checkdeploy:
	@if [ ! -x "$(CURRY2CGI)" ] ; then \
	   echo "Installing required executable 'curry-makecgi'..." ; \
           $(CPM) $(CPMOPTIONS) install html2 ; fi

# Invoke the REPL of the Curry system:
.PHONY: repl
repl:
	$(CPM) exec $(CURRYBIN)/curry --nocypm $(CURRYOPTIONS)

# Compile the generated Spicey application:
.PHONY: compile
compile:
	$(CPM) exec $(CURRYBIN)/curry --nocypm $(CURRYOPTIONS) :load Main :quit

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load:
	$(CPM) exec $(CURRYBIN)/curry --nocypm $(CURRYOPTIONS) :load Main

# Runs the generated Spicey application by evaluating the main expression.
# This might be useful to test only the initial web page without a web server
.PHONY: run
run:
	$(CPM) exec $(CURRYBIN)/curry --nocypm $(CURRYOPTIONS) :load Main :eval main :quit

# save DB in term file
.PHONY: savedata
savedata:
	cd $(MODELDIR) && $(CPM) exec $(CURRYBIN)/curry --nocypm :l MDB :eval saveDB :quit

# initialize DB from term file
.PHONY: restoredata
restoredata:
	cd $(MODELDIR) && $(CPM) exec $(CURRYBIN)/curry --nocypm :l MDB :eval restoreDB :quit

# Deploy the generated Spicey application, i.e., install it in the
# web directory WEBSERVERDIR:
.PHONY: deploy
deploy: checkdeploy
ifeq ($(TARGET),TEST)
else
	cd src/Model && $(CURRYBIN)/cleancurry ConfigMDB.curry && \
	 /bin/rm -f ConfigMDB.curry && ln -s ConfigMDB_$(TARGET).curry ConfigMDB.curry
endif
	mkdir -p $(WEBSERVERDIR)
	$(MAKE) $(CGIPROGRAM)
	# copy other files (style sheets, images,...)
	cp -r $(PKGDIR)/public/* $(WEBSERVERDIR)
	chmod -R go+rX $(WEBSERVERDIR)
ifeq ($(TARGET),TEST)
	# create directory for storing local session data:
	#/bin/rm -r $(SESSIONDATADIR)
	mkdir -p $(SESSIONDATADIR)
	chmod 700 $(SESSIONDATADIR)
else
	cd src/Model && $(CURRYBIN)/cleancurry ConfigMDB.curry && \
	 /bin/rm -f ConfigMDB.curry && ln -s ConfigMDB_TEST.curry ConfigMDB.curry
endif

$(CGIPROGRAM): src/*.curry src/*/*.curry
	$(CPM) exec $(CURRY2CGI) --cpmexec \"$(CPM) exec\" \
	  --system="$(CURRYHOME)" \
	  -i Controller.AdvisorStudyProgram \
	  -i Controller.Category \
	  -i Controller.MasterCoreArea \
	  -i Controller.ModData \
	  -i Controller.Search \
	  -i Controller.Student \
	  -i Controller.StudentCourse \
	  -i Controller.StudyProgram \
	  -i Controller.UnivisInfo \
	  -i Controller.User \
	  -o $@ Main.curry

# clean up generated the package directory
.PHONY: clean
clean: 
	$(CPM) $(CPMOPTIONS) clean

# clean everything, including the deployed files
.PHONY: cleanall
cleanall: clean
	/bin/rm -rf $(CGIPROGRAM)*
