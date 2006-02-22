BEGIN { 
	procmode = "CHECK"; 

	print ".model test"
	print ".outputs OUT"
	tmp = 1;
	tmpnm = "tmpnm"tmp;
}

{
	
	out = $1;
	nmout = "nm"$1;
	in1 = ($3)>0?$3 : -$3;
	in2 = ($4)>0?$4 : -$4;
	in3 = ($5)>0?$5 : -$5;
	sgn1 = $3>0 ?1:0; nsgn1 = 1-sgn1;
	sgn2 = $4>0 ?1:0; nsgn2 = 1-sgn2;
	sgn3 = $5>0 ?1:0; nsgn3 = 1-sgn3;
	nm1 = "nm"in1;
	nm2 = "nm"in2;
	nm3 = "nm"in3;

}
/ CHOICE/ { 
	print ".names "nm1" "nm2" "nm3" "nmout; 
	print sgn1""sgn2"- 1\n" nsgn1 "-" sgn3 " 1";
  }


/ XOR/ { 

	print ".names "nm1" "nm2" "nmout; 
	print sgn1""nsgn2" 1\n"nsgn1""sgn2" 1";	 
}

/ OR/ {  
	print ".names "nm1" "nm2" "nmout; 
	print sgn1"- 1\n-"sgn2" 1";	 
}

/ BOR/ {  
	lpla = "";
	printf(".names ");
	split($0, ar);
	for(i = 0; i<ar[3]; i = i+1){
		inv = ar[4+i];
		inx = inv>0? inv:-inv;
		sgnx = inv>0? 1:0;
	 	printf("nm%d ", inx);
		for(j = 0; j<ar[3]; j = j+1){
			if( j == i){
				lpla = lpla""sgnx;
			}else{
				lpla = lpla"-";
			}
		}
		lpla = lpla " 1\n"; 
	}
	printf(nmout"\n");
	print lpla;    
}

/ AND/ { 
	print ".names "nm1" "nm2" "nmout; 
	print sgn1""sgn2" 1";
}

/ EQ/ { 
	print ".names "nm1" "nmout; 
	print sgn1" 1";
 }

/ OUTXOR/ {
	print ".names "nm1" "nm2" "tmpnm;
	print sgn1""nsgn2" 1\n"nsgn1""sgn2" 1";	 
	outname = outname" "tmpnm;
	tmp = tmp+1;
	tmpnm = "tmpnm"tmp
  }

/SET / {
	print ".names "nm1;
	print sgn1
}


/_C/ {
	if( procmode == "FIND" ){	
		printf(".inputs ");
		lsz = split($0, ar);
		for(i = 3; i<=lsz; i = i+1){
			inx = ar[i];
			printf("nm%d ", inx);
		}
		printf("\n");
	}
}

/_IN/ {
	if( procmode == "CHECK" ){	
		printf(".inputs ");
		lsz = split($0, ar);
		for(i = 3; i<=lsz; i = i+1){
			inx = ar[i];
			printf("nm%d ", inx);
		}
		printf("\n");
	}
}


/_OUT/{




}

/_SOUT/{


}


END { 
	pla = "";	

	printf(".names "); 
	for(i = 1; i<tmp; i=i+1){
	   printf("tmpnm%d ", i);
	   pla = pla"1";
	}

	printf(" OUT\n");
	print pla" 1";

	

}
