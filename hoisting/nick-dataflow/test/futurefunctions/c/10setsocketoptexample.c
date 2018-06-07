
#include <sys/socket.h> 
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <netinet/in.h>

#define SENDER_PORT_NUM 6000 
#define SENDER_IP_ADDR "192.136.23.20" 

void main(void)
{

int socket_fd;
int option = 1; struct sockaddr_in sa;

/* Creates an TCP socket (SOCK_STREAM) with Internet Protocol Family (PF_INET).
 * Protocol family and Address family related. For example PF_INET Protocol Family and AF_INET family are coupled.
*/

socket_fd = socket(PF_INET, SOCK_STREAM, 0);

if ( socket_fd < 0 )
{

printf("socket call failed");
exit(0);
}

memset(&sa, 0, sizeof(struct sockaddr_in));
sa.sin_family = AF_INET;
sa.sin_addr.s_addr = inet_addr(SENDER_IP_ADDR);
sa.sin_port = htons(SENDER_PORT_NUM);


/* Bind the TCP socket to the port SENDER_PORT_NUM and to the current
* machines IP address (Its defined by SENDER_IP_ADDR). 
* Once bind is successful for UDP sockets application can operate
* on the socket descriptor for sending or receiving data.
*/
if (bind(socket_fd, (struct sockaddr *)&sa, sizeof(struct sockaddr_in)) == -1)
{

printf("Bind to Port Number %d ,IP address %s failed\n",SENDER_PORT_NUM,SENDER_IP_ADDR);
close(socket_fd);
exit(1);
}


// if(setsockopt(socket_fd,SOL_SOCKET,(SO_REUSEPORT | SO_REUSEADDR),(char*)&option,sizeof(option)) < 0)
if(setsockopt(socket_fd,SOL_SOCKET,IP_MULTICAST_IF,(char*)&option,sizeof(option)) < 0)
{
printf("setsockopt failed\n");
close(socket_fd);
exit(2);
}
close(socket_fd);
}