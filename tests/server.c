#include <stdlib.h>
#include <stdio.h>

#include <utp.h>
#include <utp.c>


int test()
{
    return -1;
}

int main(int argc, char ** argv)
{
    if (argc != 2) {
        fprintf(stderr, "usage: server <PORT>\n");
        return EXIT_FAILURE;
    }

    int port = atoi(argv[1]);
    printf("using port = %i\n", port);

    struct usocket * sock = usocket();
    if (sock == NULL) {
        perror("unable to open socket");
        return EXIT_FAILURE;
    }
    printf("socket opened 0x%x\n", sock);

    struct sockaddr_in serv_addr;
    bzero(&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port        = htons(port);
    int ret = ubind(sock, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    if (ret == -1) {
        perror("unable to bind socket");
        return EXIT_FAILURE;
    }
    printf("socket bound\n");

    // TODO listen

    int i = 0;
    for (; ; ++i) {
        struct sockaddr_in cli_addr;
        socklen_t len = sizeof(cli_addr);

        fprintf(stderr, "waiting for a connection\n");
        struct usocket * conn = uaccept(sock, (struct sockaddr *)&cli_addr, &len);
        if (conn == NULL) {
            perror("accept");
            continue;
        }
        fprintf(stderr, "opened %i connection\n", i);

        //pid_t pid = fork();
        int ret = uclose(conn);
        if (ret == -1) {
            perror("unable to close connection");
            return EXIT_FAILURE;
        }
    }

    ret = uclose(sock);
    if (ret == -1) {
        perror("unable to close socket");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
