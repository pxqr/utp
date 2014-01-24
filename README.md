µTP - micro transport protocol implementation. This is an stateful
transport layer protocol built on top of UDP. The protocol is similar
to TCP but use its own framing scheme and delay-based congestion
control. The library API mimic sockets API, there are socket(2),
connect(2), bind(2), listen(2), close(2), recv(2), send(2) functions.

References
==========

[uTorrent transport protocol][BEP5]
[A look inside the BitTorrent protocol][utp-paper]
[libutp HEAD][libutp]
[libtorrent utp][libtorrent]

[BEP5]:       http://www.bittorrent.org/beps/bep_0029.html
[utp-paper]:  http://norswap.eu/papers/utp.pdf
[libutp]:     https://github.com/bittorrent/libutp
[libtorrent]: http://www.libtorrent.org/utp.html
