SRC = rebalance
LIBS = cohttp.async,yojson,textwrap,netclient

$(SRC).byte: $(SRC).ml
	corebuild -pkg $(LIBS) $(SRC).byte
