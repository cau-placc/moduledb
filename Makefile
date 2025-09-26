# Makefile to test and deploy MDB

# To deploy with KiCS2, run: make SYSTEM=/opt/kics2 deploy

# Definition of the root of the Curry system to be used:
#SYSTEM=/opt/kics2
#SYSTEM=$(HOME)/pakcs

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored, e.g.: $(HOME)/public_html
WEBSERVERDIR=$(HOME)/public_html/mdbtest

# Name of the compiled cgi program
CGIPROGRAM=$(WEBSERVERDIR)/show.cgi

# Curry bin directory to be used:
export CURRYBIN=$(SYSTEM)/bin

# Default options for compiling Curry programs
CURRYOPTIONS=:set -time

# Executable of the Curry Package Manager CPM:
CPM := $(CURRYBIN)/cypm

# directory with all session data:
SESSIONDATADIR=$(WEBSERVERDIR)/sessiondata

# The root directory of the package of the Spicey application:
PKGDIR := $(CURDIR)

# The root directory of the model sources of the Spicey application:
MODELDIR := $(PKGDIR)/src/Model

# Executable of CurryPP (require to compile SQL queries):
CURRYPP := $(shell which currypp)
ifeq ("$(CURRYPP)","")
$(error Executable 'currypp' not found! Install it by 'cypm install currypp' and add '~/.cpm/bin' to your PATH)
endif

# Executable of the makecgi:
CURRY2CGI := $(shell which curry2cgi)

# all Curry source files used by the implementation
SOURCES := $(shell find src -name "*.curry") src/Model/MDB/Queries.curry

##############################################################################

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
	   echo "Installing required executable 'curry2cgi'..." ; \
           $(CPM) $(CPMOPTIONS) install html2 ; fi

# Invoke the REPL of the Curry system:
.PHONY: repl
repl:
	$(CPM) exec $(CURRYBIN)/curry --nocypm $(CURRYOPTIONS)

# Generate pure Curry module MDB.Queries with CurryPP:
src/Model/MDB/Queries.curry: src/Model/MDB/Queries.curry.pp
	rm -f $@ && cd src/Model/MDB && ln -s Queries.curry.pp Queries.curry
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Model.MDB.Queries :quit
	rm $@ && mv src/Model/MDB/Queries.curry.CURRYPP $@

# Compile the generated Spicey application:
.PHONY: compile
compile: $(SOURCES)
	$(CPM) exec $(CURRYBIN)/curry --nocypm $(CURRYOPTIONS) :load Main :quit

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load: $(SOURCES)
	$(CPM) exec $(CURRYBIN)/curry --nocypm $(CURRYOPTIONS) :load Main

# Runs the generated Spicey application by evaluating the main expression.
# This might be useful to test only the initial web page without a web server
.PHONY: run
run: $(SOURCES)
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
	mkdir -p $(WEBSERVERDIR)
	$(MAKE) $(CGIPROGRAM)
	# copy other files (style sheets, images,...)
	cp -a $(PKGDIR)/public/* $(WEBSERVERDIR)
	chmod -R go+rX $(WEBSERVERDIR)
	# create directory for storing local session data:
	#/bin/rm -r $(SESSIONDATADIR)
	mkdir -p $(SESSIONDATADIR)
	chmod 700 $(SESSIONDATADIR)

$(CGIPROGRAM): $(SOURCES)
	$(CURRY2CGI) --cpm="$(CPM)" --system="$(SYSTEM)" \
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
