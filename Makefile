BINS = dtao
MANS = doc/dtao-guile.1

PREFIX ?= /usr/local
CFLAGS += -Wall -Wextra -Wno-unused-parameter -g -DPREFIX=\"$(PREFIX)\"

all: $(BINS) $(MANS)

clean:
	$(RM) $(BINS) $(addsuffix .o,$(BINS))

install: all
	install -D -t $(PREFIX)/bin $(BINS)
	install -Dm644 -t $(PREFIX)/share/man/man1 $(MANS)
	cp -r share $(PREFIX)/share/dtao-guile

$(MANS): %: %.ronn
	ronn -r $<

WAYLAND_PROTOCOLS=$(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER=$(shell pkg-config --variable=wayland_scanner wayland-scanner)

xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) client-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

xdg-shell-protocol.c:
	$(WAYLAND_SCANNER) private-code \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

xdg-shell-protocol.o: xdg-shell-protocol.h

wlr-layer-shell-unstable-v1-protocol.h:
	$(WAYLAND_SCANNER) client-header \
		protocols/wlr-layer-shell-unstable-v1.xml $@

wlr-layer-shell-unstable-v1-protocol.c:
	$(WAYLAND_SCANNER) private-code \
		protocols/wlr-layer-shell-unstable-v1.xml $@

wlr-layer-shell-unstable-v1-protocol.o: wlr-layer-shell-unstable-v1-protocol.h

dscm-unstable-v1-protocol.h:
	$(WAYLAND_SCANNER) client-header \
		protocols/dscm-unstable-v1.xml $@

dscm-unstable-v1-protocol.c:
	$(WAYLAND_SCANNER) private-code \
		protocols/dscm-unstable-v1.xml $@

dscm-unstable-v1-protocol.o: dscm-unstable-v1-protocol.h

dtao.o: dscm/config.h dscm/utils.h dscm/bindings.h utf8.h xdg-shell-protocol.h wlr-layer-shell-unstable-v1-protocol.h dscm-unstable-v1-protocol.h

# Protocol dependencies
dtao: xdg-shell-protocol.o wlr-layer-shell-unstable-v1-protocol.o dscm-unstable-v1-protocol.o dtao

# Library dependencies
dtao: CFLAGS+=$(shell pkg-config --cflags wayland-client fcft pixman-1 guile-3.0)
dtao: LDLIBS+=$(shell pkg-config --libs wayland-client fcft pixman-1 guile-3.0) -lrt

.PHONY: all clean install
