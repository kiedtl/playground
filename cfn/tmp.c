#include <stdio.h>

char *m;

getmsg(){m="test";}

main(){
	getmsg();
	printf("test: %s\n", m);
}
