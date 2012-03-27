# Makefile for installing Lua
# See doc/readme.html for installation and customization instructions.

UNAME_S:=$(shell uname -s)
FLAVOR=Debug
VARIANT=$(UNAME_S)_$(FLAVOR)

PLAT.Darwin=macosx

PLAT=$(or $(PLAT.$(UNAME_S)),none)

# Other utilities.
MKDIR= mkdir -p
RM= rm -f
RMDIR= rm -rf

# == END OF USER SETTINGS -- NO NEED TO CHANGE ANYTHING BELOW THIS LINE =======

# Convenience platforms targets.
PLATS= aix ansi bsd freebsd generic linux macosx mingw posix solaris

# Lua version and release.
V= 5.2
R= $V.0

# Targets start here.
all:	$(PLAT) $(VARIANT)/ship/lua

$(VARIANT)/ship/lua: src/lua
	$(MKDIR) $(@D)
	cp $< $@

$(PLATS) clean:
	cd src && $(MAKE) $@

clean: bye

bye:
	$(RMDIR) $(VARIANT)

test:	dummy
	src/lua -v

none:
	@echo "Please do 'make PLATFORM' where PLATFORM is one of these:"
	@echo "   $(PLATS)"
	@echo "See doc/readme.html for complete instructions."

# make may get confused with test/ and install/
dummy:

# echo config parameters
echo:
	@cd src && $(MAKE) -s echo
	@echo "PLAT= $(PLAT)"
	@echo "V= $V"
	@echo "R= $R"

# echo pkg-config data
pc:
	@echo "version=$R"
	@echo "prefix=$(INSTALL_TOP)"
	@echo "libdir=$(INSTALL_LIB)"
	@echo "includedir=$(INSTALL_INC)"

# list targets that do not create files (but not all makes understand .PHONY)
.PHONY: all $(PLATS) clean test none dummy echo pecho lecho

# (end of Makefile)
