#include <stdlib.h>
#include <stdio.h>

#include <utp.h>
#include <utp.c>


int test(struct usocket * sock, struct sockaddr * serv_addr, socklen_t sock_len)
{
    int ret = uconnect(sock, serv_addr, sock_len);
    if (ret == -1) {
        perror("unable to connect");
        return EXIT_FAILURE;
    }
    printf("connected to TODO\n");

    const char buf[] = "hello!";
    int n = usend(sock, buf, sizeof(buf));
    if (n == -1) {
        perror("unable to send");
        return EXIT_FAILURE;
    }
    printf("sent %d bytes\n", n);
    printf("'%s' sent\n", buf);

    char bufr[5];
    n = urecv(sock, bufr, 5);
    if (n == -1) {
        perror("unable to receive");
        return EXIT_FAILURE;
    }
    printf("received %d bytes\n", n);
    printf("'%s' received\n", bufr);

    return EXIT_SUCCESS;
}

int main(int argc, char ** argv)
{
    if (argc != 3) {
        fprintf(stderr, "usage: client <IP> <PORT>\n");
        return EXIT_FAILURE;
    }

    char* ip = argv[1];
    int port = atoi(argv[2]);
    printf("using ip = %s, port = %i\n", ip, port);


    struct usocket * sock = usocket();
    if (sock == NULL) {
        perror("unable to open socket");
        return EXIT_FAILURE;
    }
    printf("socket opened 0x%x\n", sock);

    struct sockaddr_in serv_addr;
    bzero(&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_addr.s_addr = inet_addr(ip);
    serv_addr.sin_port        = htons(port);

    int ret = test(sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr));
    if (ret == EXIT_FAILURE) {
        fprintf(stderr, "error occured, but trying to close socket\n");
    }

    ret = uclose(sock);
    if (ret == -1) {
        perror("unable to close");
        return EXIT_FAILURE;
    }
    printf("socket closed 0x%x\n", sock);

    return EXIT_SUCCESS;
}
