LIBS=-lssh2
GHC=ghc $(LIBS) -isrc/ --make
C2HS=c2hs -C"-Iinclude/"
HSFILES=src/Network/SSH/Client/LibSSH2.hs src/Network/SSH/Client/LibSSH2/Types.hs src/Network/SSH/Client/LibSSH2/Errors.hs src/Network/SSH/Client/LibSSH2/Foreign.hs

all: ssh-client

ssh-client: ssh-client.hs $(HSFILES)
	$(GHC) $<

%.hs: %.chs
	$(C2HS) $<

clean:
	find . -name \*.hi -delete
	find . -name \*.chi -delete
	find . -name \*.o -delete
	find . -name \*.chs.h -delete
	rm -f ssh-client
