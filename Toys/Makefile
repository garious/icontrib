UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

all: server


.PHONY: server client
server:
	$(MAKE) -C ../Server V=$V

../Server/$V/ship/icontrib: server

serve: ../Server/$V/ship/icontrib
	$< --yoinkdir=../Yoink/$V/Ship --moddir=. --tagdir=../Tag

