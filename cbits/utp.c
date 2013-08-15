#include "utp.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <strings.h>


#include <unistd.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>


typedef uint8_t  capabilities ;
typedef uint16_t connection_id;
typedef uint32_t timestamp    ;
typedef uint32_t window_size  ;
typedef uint16_t packet_id    ;

typedef enum _packet_type {
    ST_DATA = 0,
    ST_FIN,
    ST_STATE,
    ST_RESET,
    ST_SYN,

    ST_MIN = ST_DATA, ST_MAX = ST_SYN
} packet_type;

unsigned int utp_version1 = 1;
capabilities no_ext = 0;

#pragma pack(push)
#pragma pack(1)

typedef struct _packet {
    packet_type   type    : 4;
    unsigned int  version : 4;
    capabilities  extension;

    connection_id conn_id;

    timestamp     time_sent;
    timestamp     time_diff;

    window_size   wnd_size;

    packet_id     seq_nr;
    packet_id     ack_nr;
} packet;

#pragma pack(pop)

#define STATIC_ASSERT(msg, e) enum { msg = (1/(!!(e))) };

STATIC_ASSERT(incorrect_packet_size, sizeof(packet) == 20)

char * get_packet_type_str(packet_type ty)
{
    switch (ty) {
    case ST_DATA:  return "DATA";
    case ST_FIN:   return "FIN";
    case ST_STATE: return "STATE";
    case ST_RESET: return "RESET";
    case ST_SYN:   return "SYN";
    default:       return "UNKNOWN";
    };
}

void print_packet(packet * p)
{
    fprintf(stderr, "type      = %s\n", get_packet_type_str(p->type));
    fprintf(stderr, "version   = %d\n", p->version);
    fprintf(stderr, "extension = %d\n", p->extension);
    fprintf(stderr, "conn_id   = %d\n", p->conn_id);
    fprintf(stderr, "time_sent = %d\n", p->time_sent);
    fprintf(stderr, "time_diff = %d\n", p->time_diff);
    fprintf(stderr, "wnd_size  = %d\n", p->wnd_size);
    fprintf(stderr, "seq_nr    = %d\n", p->seq_nr);
    fprintf(stderr, "ack_nr    = %d\n", p->ack_nr);
}

typedef enum _socket_status {
    NOT_CONNECTED = 0,
    CONNECTED,
    BOUND,
    LISTENING,
    CLOSED
} socket_status;

struct usocket {
    window_size wnd_size;
    window_size cur_wind;

    timestamp   reply_micro;

    // sequence number of last sent packet
    packet_id seq_nr;
    // sequence number of last acked packet
    packet_id ack_nr;

    socket_status status;

    connection_id conn_id_send;
    connection_id conn_id_recv;

    struct sockaddr  addr;
    socklen_t        addrlen;

    int fd;
};

char * get_status_str(socket_status st)
{
    switch (st) {
    case NOT_CONNECTED: return "NOT_CONNECTED";
    case CONNECTED:     return "CONNECTED";
    case BOUND:         return "BOUND";
    case LISTENING:     return "LISTENING";
    case CLOSED:        return "CLOSED";
    default:            return "INVALID";
    }
}

void print_sock_state(struct usocket * sock)
{
    fprintf(stderr, "socket is %s\n", get_status_str(sock->status));
    fprintf(stderr, "window  size = %d\n", sock->wnd_size);
    fprintf(stderr, "current size = %d\n", sock->cur_wind);
    fprintf(stderr, "reply  micro = %d\n", sock->reply_micro);
    fprintf(stderr, "sent seq  nr = %d\n", sock->seq_nr);
    fprintf(stderr, "recv seq  nr = %d\n", sock->ack_nr);
    fprintf(stderr, "send conn id = %d\n", sock->conn_id_send);
    fprintf(stderr, "recv conn id = %d\n", sock->conn_id_recv);
}

int inflight(struct usocket * sock)
{
    return 0;
}

/* NOTE according to gettimeofday(2) it's can't return (-1) in the
 following usage. so, it's safe to ignore ret value completely.  */
suseconds_t get_usec()
{
    struct timeval tv;
    int ret = gettimeofday(&tv, NULL);
    assert(ret != -1);
    return tv.tv_usec;
}

int fill_header(struct usocket * sock, packet * pkt, packet_type ty)
{
    pkt->type      = ty;
    pkt->version   = utp_version1;
    pkt->extension = no_ext;
    pkt->conn_id   = ty == ST_SYN ? sock->conn_id_recv
                                  : sock->conn_id_send;
    pkt->time_sent = get_usec();
    pkt->time_diff = sock->reply_micro;
    pkt->wnd_size  = inflight(sock);
    // FIXME do not increase seq_nr for packets with no payload
    pkt->seq_nr    = sock->seq_nr++;
    pkt->ack_nr    = sock->ack_nr;

    return 0;
}

int after_recv(struct usocket * sock, packet * pkt)
{
    bool typ_ok = (ST_MIN <= pkt->type) && (pkt->type <= ST_MAX);
    bool ver_ok = pkt->version   == utp_version1;
    bool ext_ok = pkt->extension == no_ext;
    bool cid_ok = pkt->conn_id   == sock->conn_id_recv;

    if (!(typ_ok && ver_ok && ext_ok && cid_ok)) {
        errno = EPROTO;
        return -1;
    }

    sock->reply_micro = get_usec() - pkt->time_sent;
    return 0;
}

int send_pkt(struct usocket * sock, packet * pkt)
{
    int ret = sendto(sock->fd
                    , pkt, sizeof(struct _packet), 0
                    , &(sock->addr), sock->addrlen);
    return ret;
}

int recv_pkt(struct usocket * sock, packet * pkt)
{
    struct sockaddr_in recv_addr;
    socklen_t recv_addrlen;

    int ret = recvfrom(sock->fd
                      , pkt, sizeof(struct _packet), 0
                      , (struct sockaddr *) &recv_addr, &recv_addrlen);

    if (ret != sizeof(struct _packet)) {
        errno = EPROTO;
        return -1;
    }

    // TODO check if addrs the same
    ret = after_recv(sock, pkt);
    return ret;
}

int finalize(struct usocket * sock)
{
    packet fin;
    fill_header(sock, &fin, ST_FIN);

    int ret = send_pkt(sock, &fin);
    if (ret == -1) {
        return ret;
    }

    packet ack;
    ret = recv_pkt(sock, &ack);
    if (ack.type == ST_STATE) {
        return 0;
    } else {
        return -1;
    }

    return ret;
}

struct usocket* usocket()
{
    const int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd == -1) {
        return NULL;
    }

    struct usocket * sock = malloc(sizeof(struct usocket));
    if (sock == NULL) {
        close(fd);
        errno = ENOMEM;
        return NULL;
    }

    sock->wnd_size = 1;
    sock->cur_wind = 0;
    sock->reply_micro = 0;
    sock->seq_nr = 0;
    sock->ack_nr = 0;
    sock->status = NOT_CONNECTED;
    sock->conn_id_send = 0;
    sock->conn_id_recv = 0;
    sock->fd = fd;

    return sock;
}

int uclose(struct usocket * sock)
{
    assert(sock != NULL);

    int ret1 = 0;
    if (sock->status == CONNECTED) {
      ret1 = finalize(sock);
    }

    int ret2 = close(sock->fd);
    free(sock);
    return ret1 | ret2;
}

connection_id gen_conn_id()
{
    return rand();
}

int uconnect( struct usocket * sock
            , const struct sockaddr * addr
            , socklen_t addrlen )
{
    assert(sock != NULL);

    if (sock->status == CONNECTED) {
        errno = EISCONN;
        return -1;
    }

    sock->conn_id_send = rand();
    sock->conn_id_recv = sock->conn_id_recv + 1;
    sock->addr    = *addr;
    sock->addrlen = addrlen;

    packet syn;
    fill_header(sock, &syn, ST_SYN);
    int ret = send_pkt(sock, &syn);
    if (ret == -1) {
        return ret;
    }

    // TODO ECONNREFUSED

    packet state;
    ret = recv_pkt(sock, &state);
    if (ret == -1) {
        return ret;
    }
    print_packet(&state);

    sock->status = CONNECTED;

    errno = ENOSYS;
    return -1;
}

int ubind( struct usocket * sock
         , const struct sockaddr * addr
         , socklen_t addrlen )
{
    assert(sock != NULL);

    int ret = bind(sock->fd, addr, addrlen);
    sock->status = BOUND;

    return ret;
}

int ulisten(struct usocket * sock, int backlog)
{
    assert(sock != NULL);

    sock->status = LISTENING;

    errno = ENOSYS;
    return -1;
}

bool valid_init_header(packet * hdr)
{
    return (hdr->type == ST_SYN);
}

struct usocket *
uaccept( struct usocket * sock
       , struct sockaddr * addr
       , socklen_t * addrlen)
{
    assert(sock != NULL);

    const size_t buf_len = sizeof(struct _packet);
    char buf[buf_len];
    size_t size = recvfrom( sock->fd, buf, buf_len, 0
                          , (struct sockaddr *) addr, addrlen);
    if (size != buf_len) {
        errno = EPROTO;
        return NULL;
    }
    packet * init_hdr = (packet *)buf;
    print_packet(init_hdr);

    if (!valid_init_header(init_hdr)) {
        errno = EPROTO;
        return NULL;
    }

    struct usocket * conn = usocket();
    if (conn == NULL) {
        return NULL;
    }
    conn->conn_id_send = init_hdr->conn_id;
    conn->conn_id_recv = init_hdr->conn_id + 1;
    conn->seq_nr       = rand();
    conn->ack_nr       = init_hdr->seq_nr;
    conn->status       = CONNECTED;

    packet st;
    fill_header(conn, &st, ST_STATE);
    size = sendto( sock->fd, &st, sizeof(st), 0
                 , addr, *addrlen);

    return conn;
}

int urecv(struct usocket * sock, void * buf, size_t len)
{
    assert(sock != NULL);
    errno = ENOSYS;
    return -1;
}

int usend(struct usocket * sock, const void * buf, size_t len)
{
    assert(sock != NULL);

    packet dat;
    fill_header(sock, &dat, ST_DATA);

    errno = ENOSYS;
    return -1;
}
