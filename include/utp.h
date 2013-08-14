#ifndef _UTP_H_
#define _UTP_H_

#include <stdlib.h>
#include <sys/socket.h>

struct usocket;

struct usocket * usocket();
int uclose(struct usocket * sock);

int uconnect(struct usocket * sock
            , const struct sockaddr * addr
            , socklen_t addrlen);
int ubind( struct usocket * sock
           , const struct sockaddr * addr
           , socklen_t addrlen );

int ulisten(struct usocket * sock, int backlog);

struct usocket *
uaccept( struct usocket * sock
       , struct sockaddr * addr
       , socklen_t * addrlen);

int urecv(struct usocket * sock, void * buf, size_t len);
int usend(struct usocket * sock, const void * buf, size_t len);

#endif /* _UTP_H_ */
