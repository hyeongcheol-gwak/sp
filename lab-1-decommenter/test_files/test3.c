#include /*Header File 1*/<stdio.h>
#include /*Header File 2*/<stdlib.h> 

/* Program Author: Soowan Park */
/*
 * / * Prints Hello World * /
 */

int main( int argc, char** argv ) {

	printf( "\"Hello World!\"*\n" );
	printf( "/\"Hello World!\"**\n"  /*This should be 
		fine*/
		);



	printf( '/***\"Hello World!\"*\n' );
	printf( '*\"Hello World! \"***/\n'  /*This should be 
		fine*/
		);

	return EXIT_SUCCESS;
}
