%{



using namespace std;

BooleanDAG* currentBD;
stack<int> sgn_stack;
stack<string> namestack;



string* comparisson(string* p1, string* p2, arith_node::AType atype){

int t2 = sgn_stack.top(); sgn_stack.pop();
int t1 = sgn_stack.top(); sgn_stack.pop();
string s1 = currentBD->new_name();
arith_node* an = new arith_node();
an->arith_type = atype;
if(p1== NULL){
	an->mother_quant = t1;
	an->father_quant = t2;
	//cout<<"Mq = "<<t2<<"  FQ= "<<t1<<endl;
	Assert(p2 != NULL, "THIS CAN't Happen");
	currentBD->new_node("", true, *p2, true, bool_node::ARITH, s1, an); 
}else if(p2==NULL){
	an->mother_quant = t1;
	an->father_quant = t2;
	//cout<<"Mq = "<<t1<<"  FQ= "<<t2<<endl;
	Assert(p1 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*p1, true, "", true, bool_node::ARITH, s1, an); 
}else{
	an->mother_quant = t1;
	an->father_quant = t2;
	Assert(p1 != NULL && p2 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*p1, true, *p2, true, bool_node::ARITH, s1, an); 
}
sgn_stack.push(true);
return new string(s1); 


}




%}


%union {
	int intConst;
	bool boolConst;
	std::string* strConst;
	double doubleConst;		
	std::list<int>* iList;
	list<bool_node*>* nList;
	vartype variableType;
}

%token <doubleConst> T_dbl
%token<intConst>  T_int
%token<intConst>  T_bool
%token<strConst> T_ident               
%token<strConst> T_OutIdent
%token<strConst> T_NativeCode
%token<strConst> T_string
%token<boolConst> T_true
%token<boolConst> T_false
%token<variableType> T_vartype
%token T_ppls
%token T_mmns
%token T_eq
%token T_neq
%token T_and
%token T_or
%token T_For
%token T_ge
%token T_le

%token T_Table
%token T_Pipeline
%token T_SplitJoin
%token T_Filter 
%token T_Native
%token T_NativeMethod
%token T_Work
%token T_Sketches
%token T_OutRate
%token T_new
%token T_InRate
%token T_add
%token T_Init
%token T_setSplitter
%token T_setJoiner

%token T_eof
%type<intConst> InitBody
%type<intConst> Program
%type<strConst> Ident
%type<intConst> FilterList
%type<intConst> WorkStatement
%type<strConst> Expression
%type<strConst> Term
%type<intConst> Constant
%type<intConst> ConstantExpr
%type<intConst> ConstantTerm
%type<nList> varList


%left '+'
%left '*'
%left '/'
%left '%'
%left '<'
%left '>'
%left T_eq
%left T_neq
%right '?'
%right ':'


%%

Program: FilterList T_eof{ cout<<"Program"<<endl; $$=0; return 0;}


FilterList: Filter  { }
| FilterList Filter { $$ = 0; }


Filter: FilterType T_ident  {namestack.push(*$2); } '{' MethodList '}' { 
				namestack.pop();
			}


FilterType: T_Filter { }



MethodList: Method	{}
| MethodList Method	{}


InList: T_ident {  currentBD->create_inputs(-1, *$1); }
| T_ident InList {
	
	currentBD->create_inputs(-1, *$1);
}

OutList: T_ident { 	 currentBD->create_outputs(-1, *$1); }
| T_ident OutList{
	
	currentBD->create_outputs(-1, *$1);
}


ParamDecl: T_vartype T_ident {  currentBD->create_inputs(-1, *$2); }
| '!' T_vartype T_ident { 	 currentBD->create_outputs(-1, *$3); }
| T_vartype '[' ConstantExpr ']' InList 
| '!' T_vartype '[' ConstantExpr ']' OutList 


ParamList: ParamDecl 
| ParamDecl ',' ParamList 


InitBody:  { /* Empty */ }
| InitBody InitStatement { /* */ }


InitStatement:   ';' { /* */ }
| T_InRate '=' T_int ';'
| T_OutRate '=' T_int ';'
| T_ident '(' T_ident '.' T_ident ',' T_ident '.' T_ident ')' ';'

Method: T_Init '(' ')' '{' InitBody '}' 
| T_Work '(' ')' '{' 
	{
		if( currentBD != NULL){
			//currentBD->print(cout);
		}
		currentBD = new BooleanDAG();
		functionMap["work"] = currentBD;
	}
	WorkBody '}' { }

| T_ident 

{
		if( currentBD != NULL){
			//currentBD->print(cout);
		}
		currentBD = new BooleanDAG();
		cout<<"CREATING "<<*$1<<endl;
		functionMap[*$1] = currentBD;
}
'(' ParamList ')' '{' WorkBody '}'


|T_ident T_Sketches T_ident 

{
		if( currentBD != NULL){
			//currentBD->print(cout);
		}
		currentBD = new BooleanDAG();
		cout<<*$1<<" SKETCHES "<<*$3<<endl;
		sketchMap[*$1] = currentBD;
		sketches[currentBD] = *$3;
}
'(' ParamList ')' '{' WorkBody '}'





WorkBody:  { /* Empty */ }
| WorkBody WorkStatement { /* */ }


WorkStatement:  ';' {  $$=0;  /* */ }

| T_ident '=' Expression ';' { 	if( $3 == NULL){
									currentBD->alias( *$1, 1==sgn_stack.top(), "");
								}else{
									currentBD->alias( *$1, 1==sgn_stack.top(), *$3);
								}
								sgn_stack.pop();
								delete $3;
								delete $1;
							  }	
| RateSet {}
| T_OutIdent '=' Expression ';' {
								if( $3 == NULL){
									currentBD->new_node("", 1==sgn_stack.top(), "", true,  bool_node::DST, *$1);
								}else{
									currentBD->new_node(*$3, 1==sgn_stack.top(), "", true,  bool_node::DST, *$1);
								}
								sgn_stack.pop();
								delete $3;
								delete $1;
							  }

RateSet: T_InRate '=' T_int ';'	{ currentBD->create_inputs($3); }
| T_OutRate '=' T_int ';'   { currentBD->create_outputs($3); }


Expression: Term { $$ = $1; }
| Term '&' Term { 	bool b2 = 1==sgn_stack.top(); sgn_stack.pop();
					bool b1 = 1==sgn_stack.top(); sgn_stack.pop();					
					if( $1 != NULL && $3 != NULL){
						if( *$1 == *$3){
							if( b1 == b2){
								delete $3;
								sgn_stack.push( b2 );
								$$ = $1;
							}else{
								sgn_stack.push(false);
								$$ = NULL;
							}
						}else{
							string s = currentBD->new_name();					  
							currentBD->new_node(*$1, b1, *$3, b2,  bool_node::AND, s); 
							sgn_stack.push( true );
							$$ = new string(s);
							delete $1;
							delete $3;
						}
					}else{
						if( $1 == NULL){
							if( $3 != NULL && b1){
								$$ = $3;
							}else{
								if( $3 != NULL) delete $3;
								$$ = NULL;
							}								
							sgn_stack.push(b1 && b2);							
						}else{							
							if( $1 != NULL && b2){
								$$ = $1;
							}else{
								delete $1;
								$$ = NULL;
							}		
							sgn_stack.push(b1 && b2);							
						}
					}			  					  
				} 
| Term T_and Term{ 	bool b2 = 1==sgn_stack.top(); sgn_stack.pop();
					bool b1 = 1==sgn_stack.top(); sgn_stack.pop();					
					if( $1 != NULL && $3 != NULL){
						if( *$1 == *$3){
							if( b1 == b2){
								delete $3;
								sgn_stack.push( b2 );
								$$ = $1;
							}else{
								sgn_stack.push(false);
								$$ = NULL;
							}
						}else{
							string s = currentBD->new_name();					  
							currentBD->new_node(*$1, b1, *$3, b2,  bool_node::AND, s); 
							sgn_stack.push( true );
							$$ = new string(s);
							delete $1;
							delete $3;
						}
					}else{
						if( $1 == NULL){
							if( $3 != NULL && b1){
								$$ = $3;
							}else{
								if( $3 != NULL) delete $3;
								$$ = NULL;
							}								
							sgn_stack.push(b1 && b2);							
						}else{							
							if( $1 != NULL && b2){
								$$ = $1;
							}else{
								delete $1;
								$$ = NULL;
							}		
							sgn_stack.push(b1 && b2);							
						}
					}			  					  
				}
| Term '|' Term { 	bool b2 = 1==sgn_stack.top(); sgn_stack.pop();
					bool b1 = 1==sgn_stack.top(); sgn_stack.pop();

					if( $1 != NULL && $3 != NULL){
						string s = currentBD->new_name();					  
						currentBD->new_node(*$1, b1, *$3, b2,  bool_node::OR, s); 
						sgn_stack.push( true );
						$$ = new string(s);
						delete $1;
						delete $3;
					}else{
						if( $1 == NULL){
							if( $3 != NULL && !b1){
								$$ = $3;
							}else{
								if( $3 != NULL) delete $3;
								$$ = NULL;
							}			
							sgn_stack.push(b1 || b2);
						}else{							
							if( $1 != NULL && !b2){
								$$ = $1;
							}else{
								delete $1;
								$$ = NULL;
							}
							sgn_stack.push(b1 || b2);							
						}
					}			  					  
				}
| Term T_or Term { 	bool b2 = 1==sgn_stack.top(); sgn_stack.pop();
					bool b1 = 1==sgn_stack.top(); sgn_stack.pop();					
					if( $1 != NULL && $3 != NULL){
						string s = currentBD->new_name();					  
						currentBD->new_node(*$1, b1, *$3, b2,  bool_node::OR, s); 
						sgn_stack.push( true );
						$$ = new string(s);
						delete $1;
						delete $3;
					}else{
						if( $1 == NULL){
							if( $3 != NULL && !b1){
								$$ = $3;
							}else{
								if( $3 != NULL) delete $3;
								$$ = NULL;
							}			
							sgn_stack.push(b1 || b2);
						}else{							
							if( $1 != NULL && !b2){
								$$ = $1;
							}else{
								delete $1;
								$$ = NULL;
							}
							sgn_stack.push(b1 || b2);							
						}
					}			  					  
				}
| Term '^' Term{ 	bool b2 = 1==sgn_stack.top(); sgn_stack.pop();
					bool b1 = 1==sgn_stack.top(); sgn_stack.pop();					
					if( $1 != NULL && $3 != NULL){
						string s = currentBD->new_name();
						currentBD->new_node(*$1, b1, *$3, b2,  bool_node::XOR, s);
						sgn_stack.push( true );
						$$ = new string(s);
						delete $1;
						delete $3;
					}else{
						if( $1 == NULL){
							if( $3 != NULL){
								$$ = $3;
							}else{
								delete $3;								
								$$ = NULL;
							}
							sgn_stack.push(b1 != b2);
						}else{							
							if( $1 != NULL){
								$$ = $1;
							}else{
								delete $1;
								$$ = NULL;
							}
							sgn_stack.push(b1 != b2);
						}
					} 
				}
| Term T_neq Term{ 	bool b2 = 1==sgn_stack.top(); sgn_stack.pop();
					bool b1 = 1==sgn_stack.top(); sgn_stack.pop();					
					if( $1 != NULL && $3 != NULL){
						string s = currentBD->new_name();
						currentBD->new_node(*$1, b1, *$3, b2,  bool_node::XOR, s);
						sgn_stack.push( true );
						$$ = new string(s);
						delete $1;
						delete $3;
					}else{
						if( $1 == NULL){
							if( $3 != NULL){
								$$ = $3;
							}else{
								delete $3;
								$$ = NULL;
							}
							sgn_stack.push(b1 != b2);
						}else{
							if( $1 != NULL){
								$$ = $1;
							}else{
								delete $1;
								$$ = NULL;
							}
							sgn_stack.push(b1 != b2);
						}
					} 
				}
| Term T_eq Term { 	bool b2 = 1==sgn_stack.top(); sgn_stack.pop();
					bool b1 = 1==sgn_stack.top(); sgn_stack.pop();					
					if( $1 != NULL && $3 != NULL){
						string s = currentBD->new_name();
						currentBD->new_node(*$1, b1, *$3, b2,  bool_node::XOR, s);
						sgn_stack.push( false );
						$$ = new string(s);
						delete $1;
						delete $3;
					}else{
						if( $1 == NULL){
							if( $3 != NULL){
								$$ = $3;
							}else{
								delete $3;					
								$$ = NULL;
							}
							sgn_stack.push(b1 == b2);
						}else{							
							if( $1 != NULL){
								$$ = $1;
							}else{
								delete $1;
								$$ = NULL;
							}
							sgn_stack.push(b1 == b2);
						}
					} 
				}
| '$' varList '$' '[' Expression ']' {
bool b1 = sgn_stack.top(); sgn_stack.pop();
string s1 = currentBD->new_name();
	arith_node* an = new arith_node();
	list<bool_node*>* childs = $2;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	vector<int> tempsgn(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
		if(*it != NULL){
			(*it)->children.push_back(an);
		}
		tempsgn[bigN-1-i] = sgn_stack.top();
		sgn_stack.pop();
	}
	
	for(int i=0; i<bigN; ++i){
		an->multi_mother_sgn.push_back(tempsgn[i]);
	}
	an->arith_type = arith_node::ARRACC;
	Assert($5 != NULL, "THIS CAN'T HAPPEN!!");
	currentBD->new_node(*$5, b1, "", false, bool_node::ARITH, s1, an); 
	$$ = new string(s1);  sgn_stack.push(true);
	delete childs;
}

| '$''$' varList '$''$' {
string s1 = currentBD->new_name();
	arith_node* an = new arith_node();
	list<bool_node*>* childs = $3;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	vector<int> tempsgn(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
		if(*it != NULL){
			(*it)->children.push_back(an);
		}
		tempsgn[bigN-1-i] = sgn_stack.top();
		sgn_stack.pop();
	}
	
	for(int i=0; i<bigN; ++i){
		an->multi_mother_sgn.push_back(tempsgn[i]);
	}
	an->arith_type = arith_node::ACTRL;
	currentBD->new_node("", false, "", false, bool_node::ARITH, s1, an); 
	$$ = new string(s1);  sgn_stack.push(true);
	delete childs;
}

| Term '+' Term {
int t2 = sgn_stack.top(); sgn_stack.pop();
int t1 = sgn_stack.top(); sgn_stack.pop();
string s1 = currentBD->new_name();
arith_node* an = new arith_node();
an->arith_type = arith_node::PLUS;
if($1== NULL && $3 != NULL){
	an->mother_quant = t2;
	an->father_quant = t1;
	Assert($3 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*$3, true, "", true, bool_node::ARITH, s1, an); 
}else if($3==NULL && $1 != NULL){
	an->mother_quant = t1;
	an->father_quant = t2;
	Assert($1 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*$1, true, "", true, bool_node::ARITH, s1, an); 
}else{
	an->mother_quant = t1;
	an->father_quant = t2;
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen, const + const should have been taken care of by frontend.");
	currentBD->new_node(*$1, true, *$3, true, bool_node::ARITH, s1, an); 
}
$$ = new string(s1);  sgn_stack.push(true);
}
| Term '*' Term {
int t2 = sgn_stack.top(); sgn_stack.pop();
int t1 = sgn_stack.top(); sgn_stack.pop();
string s1 = currentBD->new_name();
arith_node* an = new arith_node();
an->arith_type = arith_node::TIMES;
if($1== NULL && $3 != NULL){
	an->mother_quant = t2;
	an->father_quant = t1;
	Assert($3 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*$3, true, "", true, bool_node::ARITH, s1, an); 
}else if($3==NULL && $1 != NULL){
	an->mother_quant = t1;
	an->father_quant = t2;
	Assert($1 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*$1, true, "", true, bool_node::ARITH, s1, an); 
}else{
	an->mother_quant = t1;
	an->father_quant = t2;
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen, const + const should have been taken care of by frontend.");
	currentBD->new_node(*$1, true, *$3, true, bool_node::ARITH, s1, an); 
}
$$ = new string(s1);  sgn_stack.push(true);
}
| Term '-' Term {
int t2 = sgn_stack.top(); sgn_stack.pop();
int t1 = sgn_stack.top(); sgn_stack.pop();
string s1 = currentBD->new_name();
arith_node* an = new arith_node();
an->arith_type = arith_node::PLUS;
if($1== NULL){
	an->mother_quant = -t2;
	an->father_quant = t1;
	Assert($3 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*$3, true, "", true, bool_node::ARITH, s1, an); 
}else if($3==NULL){
	an->mother_quant = t1;
	an->father_quant = -t2;
	Assert($1 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*$1, true, "", true, bool_node::ARITH, s1, an); 
}else{
	an->mother_quant = t1;
	an->father_quant = -t2;
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen");
	currentBD->new_node(*$1, true, *$3, true, bool_node::ARITH, s1, an); 
}
$$ = new string(s1);  sgn_stack.push(true);
}
| Term '>' Term {
	$$ = comparisson($1, $3, arith_node::GT);
}
| Term '<' Term {
	$$ = comparisson($1, $3, arith_node::LT);
}
| Term T_ge Term {
	$$ = comparisson($1, $3, arith_node::GE);
}
| Term T_le Term {
	$$ = comparisson($1, $3, arith_node::LE);
}


| Expression '?' Expression ':' Expression { 	
							int i3 = sgn_stack.top(); sgn_stack.pop();
							int i2 = sgn_stack.top(); sgn_stack.pop();
							bool b3 = 1== i3;
							bool b2 = 1== i2;
							bool b1 = 1==sgn_stack.top(); sgn_stack.pop();
							if( $1 != NULL && $3 != NULL && $5 != NULL){
								string s1 = currentBD->new_name();			  
								currentBD->new_node(*$1, b1, *$3, b2,  bool_node::AND, s1);
								
								string s2 = currentBD->new_name();
								currentBD->new_node(*$1, !b1, *$5, b3,  bool_node::AND, s2);

								string s3 = currentBD->new_name();
								currentBD->new_node(s1, true, s2, true,  bool_node::OR, s3);

								sgn_stack.push( true );
								$$ = new string(s3);
								delete $1;
								delete $3;
								delete $5;
							}else{
								if( $1 == NULL){									
									if( $3 != NULL && b1){
										if( $5 != NULL ) delete $5;
										$$ = $3;
									}else if($5 != NULL && !b1){
										if( $3 != NULL ) delete $3;
										$$ = $5;
									}else{
										$$ = NULL;
										if( $3 != NULL ) delete $3;
										if( $5 != NULL ) delete $5;
									}
									sgn_stack.push( (b1 && b2) || (!b1 && b3) );
								}else{									
									if( $3 != NULL){
										//in this case, $5 == NULL
										if( b3){
											string s1 = currentBD->new_name();
											currentBD->new_node(*$1, !b1, *$3, b2,  bool_node::OR, s1);
											$$ = new string(s1);
											sgn_stack.push(true);
										}else{
											string s1 = currentBD->new_name();
											currentBD->new_node(*$1, b1, *$3, b2,  bool_node::AND, s1);
											$$ = new string(s1);
											sgn_stack.push(true);
										}
									}else{
										//$5 may or may not equal null, but $3 is null and $1 is not.
										if( b2 ){
											if( $5 != NULL){
												string s1 = currentBD->new_name();
												currentBD->new_node(*$1, b1, *$5, b3,  bool_node::OR, s1);
												$$ = new string(s1);
												sgn_stack.push( true );
											}else{
												if(  b3 ){
													$$ = NULL;
													sgn_stack.push( (b1 && b2) || (!b1 && b3) );
												}else{
													$$ = $1;
													sgn_stack.push( (b1 && b2) || (!b1 && b3) );
												}
											}
										}else{
											if( $5 != NULL){
												string s1 = currentBD->new_name();
												currentBD->new_node(*$1, !b1, *$5, b3,  bool_node::AND, s1);
												$$ = new string(s1);
												sgn_stack.push( true );
											}else{
												if(  !b3 ){
													$$ = NULL;
													sgn_stack.push( (b1 && b2) || (!b1 && b3) );
												}else{
													$$ = $1;
													sgn_stack.push( (b1 && b2) || (!b1 && b3) );
												}
											}
											
										}
									}																		
								}
							}			  					  
						}      


varList: Term {
	$$ = new list<bool_node*>();	
	if($1 != NULL){
		$$->push_back( currentBD->get_node(*$1) );
	}else{
		$$->push_back( NULL );
	}
}
| Term varList{
//The signs are already in the stack by default. All I have to do is not remove them.
	if($1 != NULL){
		$2->push_back( currentBD->get_node(*$1) );
	}else{
		$2->push_back( NULL );
	}
	$$ = $2;
}



Term: Constant {
				 $$ = NULL; 
				sgn_stack.push($1); }
| '!' Term { 
				bool tmp = 1==sgn_stack.top(); 
				sgn_stack.pop();  
				sgn_stack.push(!tmp); 
				$$ = $2;}
| '(' Expression ')' { 
						$$ = $2; 
						}
| Ident { 
			if( !currentBD->has_alias(*$1) ){ 
				$$ = $1;  sgn_stack.push(true); 
			}else{ 
				pair<string, bool> alias(currentBD->get_alias(*$1)); 
				$$ = new string( alias.first ); 
				sgn_stack.push( alias.second );  
			} 
		}
| '<' Ident '>' {
	currentBD->create_controls(-1, *$2);
	if( !currentBD->has_alias(*$2) ){ 
		$$ = $2;  sgn_stack.push(true); 
	}else{ 
		pair<string, bool> alias(currentBD->get_alias(*$2)); 
		$$ = new string( alias.first ); 
		sgn_stack.push( alias.second );  
	} 
}
| '<' Ident Constant '>' {
	int nctrls = $3;
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	int N=currentBD->create_controls(nctrls, *$2);
	arith_node* an = new arith_node();
	for(int i=0; i<nctrls; ++i){
		bool_node* tmp = (bool_node*)(*currentBD)[N-nctrls+i];
		an->multi_mother.push_back(tmp);
		tmp->children.push_back(an);		
		an->multi_mother_sgn.push_back(true);
	}
	an->arith_type = arith_node::ACTRL;
	currentBD->new_node("", false, "", false, bool_node::ARITH, *$2, an); 
	$$ = $2;  sgn_stack.push(true); 

}
| '<' Ident Constant '*' '>' {
	int N=currentBD->create_controls($3, *$2);
	arith_node* an = new arith_node();
	for(int i=0; i<$3; ++i){
		bool_node* tmp = (bool_node*)(*currentBD)[N-$3+i];
		an->multi_mother.push_back(tmp);
		tmp->children.push_back(an);		
		an->multi_mother_sgn.push_back(true);
	}
	an->arith_type = arith_node::ACTRL;
	currentBD->new_node("", false, "", false, bool_node::ARITH, *$2, an); 
	$$ = $2;  sgn_stack.push(true); 

}

ConstantExpr: ConstantTerm { $$ = $1; }
| ConstantExpr '+' ConstantTerm { $$ = $1 + $3; }
| ConstantExpr '-' ConstantTerm { $$ = $1 - $3; }

ConstantTerm: Constant { $$ = $1; }
| '(' ConstantTerm ')' { $$ = $2; }
| ConstantTerm '*' ConstantTerm { $$ = $1 * $3; } 
| ConstantTerm '/' ConstantTerm { Assert( $3 != 0, "You are attempting to divide by zero !!");
							      $$ = $1 / $3; } 
| ConstantTerm '%' ConstantTerm { Assert( $3 != 0, "You are attempting to mod by zero !!");
							      $$ = $1 % $3; }

Constant: T_int {  $$ = $1; }
| T_true { $$ = 1; }
| T_false { $$ = 0; }

Ident: T_ident { $$=$1; }

%%


void Inityyparse(){

	 	
}

void yyerror(char* c){
	Assert(false, c); 
}


int isatty(int i){



return 1;
}
