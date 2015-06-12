#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>

unsigned char *gen_rdm_bytestream (long num_bytes)
{
  unsigned char *stream = malloc (num_bytes);
  long i;

  for (i = 0; i < num_bytes; i++)
  {
    stream[i] = rand ();
  }

  return stream;
}

int main(void)
{
  int i;
  long sizeBytes = 1000000;
  int sockfd = 0,n = 0;
  char sendBuff[sizeBytes];
  struct sockaddr_in serv_addr;

  if((sockfd = socket(AF_INET, SOCK_STREAM, 0))< 0)
    {
      printf("\n Error : Could not create socket \n");
      return 1;
    }

  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(4343);
  serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");

  if(connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr))<0)
    {
      printf("\n Error : Connect Failed \n");
      return 1;
    }

    printf("\n connected \n");
    for (i = 0; i < 1000; i++) {
//        strcpy(sendBuff, gen_rdm_bytestream(sizeBytes));
        write(sockfd, gen_rdm_bytestream(sizeBytes), sizeBytes);
    }
    printf("\n sent \n");

  return 0;
}
