/* simple C++ integration test */

#include<iostream>
#include<fstream>
#include<string>
#include"rasm.h"

#include<string.h>
#include<stdio.h>
#include<stdlib.h>

using namespace std;

int main(int argc, char **argv)
{
	unsigned char *dataout=NULL;
	int lenout=0;
	int ok;

	std::cout<<"test RasmOBJ c++\n";

	if (argc==2) {
		ifstream fichier(argv[1],ios::in);
		if (fichier) {
			string meslignes;

			std::cout<<"lecture c++\n";
			getline(fichier,meslignes,'\0');
			fichier.close();

			std::cout<<"compilation de "<<argv[1]<<" de taille lue "<<meslignes.size()<<" byte(s)\n";
			char *mystr=new char[meslignes.length()+1];
			strcpy(mystr,meslignes.c_str());
			ok=RasmAssemble(mystr, meslignes.size(), &dataout, &lenout);
			ok=RasmAssemble(mystr, meslignes.size(), &dataout, &lenout);
			ok=RasmAssemble(mystr, meslignes.size(), &dataout, &lenout);
			ok=RasmAssemble(mystr, meslignes.size(), &dataout, &lenout);
			delete [] mystr;
			if (!ok) {
				std::cout<<"compilation ok\n"<<lenout<<" byte(s)\n";
				free(dataout);
			} else {
				std::cout<<"compilation aborted\n";
				return 1;
			}
		} else {
			std::cout<<"error opening file\n";
			return 1;
		}
	} else {
		std::cout<<"need a filename\n";
	}
	return 0;
}




