µTP - micro transport protocol implementation. This is an stateful
transport layer protocol built on top of UDP. The protocol is similar
to TCP but use its own framing scheme and delay-based congestion
control. The library API mimic sockets API, there are socket(2),
connect(2), bind(2), listen(2), close(2), recv(2), send(2) functions.
