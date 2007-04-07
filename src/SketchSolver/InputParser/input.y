%{

using namespace std;

BooleanDAG* currentBD;
stack<string> namestack;


string *comparisson (string *p1, string *p2, arith_node::AType atype)
{
    Assert (p1 || p2, "Can't have both comparisson's children NULL");
   
    string s1 = currentBD->new_name();
    arith_node *an = newArithNode(atype);
    an->name = s1;
    currentBD->new_node((p1 ? *p1 : ""), 
			(p2 ? *p2 : ""), an); 
    if (p1)
	delete p1;
    if (p2)
	delete p2;
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
	list<string*>* sList;
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
%token T_twoS
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

%token T_assert

%token T_eof
%type<intConst> InitBody
%type<intConst> Program
%type<strConst> Ident
%type<intConst> FilterList
%type<intConst> WorkStatement
%type<strConst> Expression
%type<strConst> Term
%type<intConst> NegConstant
%type<intConst> Constant
%type<intConst> ConstantExpr
%type<intConst> ConstantTerm
%type<nList> varList
%type<sList> IdentList


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


ParamDecl: T_vartype T_ident {  
	if( $1 == INT){
		cout<<" INPUT IS INT "<<*$2<<endl;
		currentBD->create_inputs( 2 /*NINPUTS*/ , *$2); 
	}else{
		cout<<" INPUT IS BIT "<<*$2<<endl;
		currentBD->create_inputs(-1, *$2); 
	}	
}
| '!' T_vartype T_ident {
 	 if( $2 == INT){
 	 	cout<<" OUTPUT IS INT "<<*$3<<endl;
		 currentBD->create_outputs(NINPUTS, *$3);
 	 }else{
	 	 cout<<" OUTPUT IS BIT "<<*$3<<endl;
	 	 currentBD->create_outputs(-1, *$3); 
 	 }
 }
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
		delete $3;
		delete $1;
}
'(' ParamList ')' '{' WorkBody '}'





WorkBody:  { /* Empty */ }
| WorkBody WorkStatement { /* */ }


WorkStatement:  ';' {  $$=0;  /* */ }
| T_ident '=' Expression ';' {
	currentBD->alias( *$1, *$3);
	delete $3;
	delete $1;
}							  
| '$' IdentList T_twoS varList '$''[' Expression ']' '=' Expression ';' {

	list<string*>* childs = $2;
	list<string*>::reverse_iterator it = childs->rbegin();
	
	list<bool_node*>* oldchilds = $4;
	list<bool_node*>::reverse_iterator oldit = oldchilds->rbegin();
	
	bool_node* rhs;
	rhs = currentBD->get_node(*$10);
	int bigN = childs->size();
	Assert( bigN == oldchilds->size(), "This can't happen");	

	for(int i=0; i<bigN; ++i, ++it, ++oldit){
		string s1 = currentBD->new_name();
		ARRASS_node* an = dynamic_cast<ARRASS_node*>(newArithNode(arith_node::ARRASS));
		an->multi_mother.reserve(2);
		an->multi_mother.push_back(*oldit);			
		an->multi_mother.push_back(rhs);
		an->name = s1;
		Assert( rhs != NULL, "AAARRRGH This shouldn't happen !!");
		Assert($7 != NULL, "1: THIS CAN'T HAPPEN!!");
		currentBD->new_node(*$7,  "",  an);
		currentBD->alias( *(*it), s1);
		an->quant = i;
		delete *it;
	}
	delete childs;
	delete oldchilds;
	delete $7;
	delete $10;
}
| RateSet {}
| T_OutIdent '=' Expression ';' {
	currentBD->new_node(*$3,  "",  bool_node::DST, *$1);
	delete $3;
	delete $1;
}
| T_assert Expression ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */
//    cout << "Generating assertion node..." << endl;
    string s = currentBD->new_name ();
    currentBD->new_node (*$2, "", bool_node::ASSERT, s);
//    cout << "Assertion node created, name=" << s << endl;
    delete $2;
  }
} 
| T_assert Expression ':' T_string ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */
//    cout << "Generating assertion node..." << endl;
    string s = currentBD->new_name ();
    bool_node* bn = currentBD->new_node (*$2, "", bool_node::ASSERT, s);
    dynamic_cast<ASSERT_node*>(bn)->setMsg(*$4);
//    cout << "Assertion node created, name=" << s << endl;
    delete $2;
    delete $4;
  }
} 


RateSet: T_InRate '=' T_int ';'	{ currentBD->create_inputs($3); }
| T_OutRate '=' T_int ';'   { currentBD->create_outputs($3); }


Expression: Term { $$ = $1; }
| Term '&' Term {
	string s = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::AND, s);
	$$ = new string(s);
	delete $1;
	delete $3;	  					  
}
| Term T_and Term{
	string s = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::AND, s);
	$$ = new string(s);
	delete $1;
	delete $3;	  					  
}
| Term '|' Term {
	string s = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::OR, s);
	$$ = new string(s);
	delete $1;
	delete $3;		  					  
}
| Term T_or Term { 	
	string s = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::OR, s);
	$$ = new string(s);
	delete $1;
	delete $3;
}
| Term '^' Term{	
	string s = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::XOR, s);
	$$ = new string(s);
	delete $1;
	delete $3;
}
| Term T_neq Term{ 
	bool_node* lChild=NULL;
	bool_node* rChild=NULL;
	bool_node* bn = currentBD->get_node(*$1);
	lChild = dynamic_cast<bool_node*>(bn);
	bn = currentBD->get_node(*$3);
	rChild = dynamic_cast<bool_node*>(bn);			
	string* tmp = comparisson($1, $3, arith_node::EQ);
    string s = currentBD->new_name ();
    currentBD->new_node (*tmp, "", bool_node::NOT, s);
    delete tmp;
    $$ = new string(s);
}
| Term T_eq Term { 	
	bool_node* lChild=NULL;
	bool_node* rChild=NULL;
	bool_node* bn = currentBD->get_node(*$1);
	lChild = dynamic_cast<bool_node*>(bn);
	bn = currentBD->get_node(*$3);
	rChild = dynamic_cast<bool_node*>(bn);			
	$$ = comparisson($1, $3, arith_node::EQ);
}
| '$' varList '$' '[' Expression ']' {
	int pushval = 0;
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::ARRACC);
	list<bool_node*>* childs = $2;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}		
	Assert($5 != NULL, "2: THIS CAN'T HAPPEN!!");
	an->name = s1;
	currentBD->new_node(*$5, "",  an); 
	$$ = new string(s1);
	delete childs;
	delete $5;
}

| T_twoS varList T_twoS {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::ACTRL);
	list<bool_node*>* childs = $2;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}	
	an->name = s1;
	currentBD->new_node("", "", an); 
	$$ = new string(s1);  
	delete childs;
}

| Term '+' Term {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::PLUS);
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*$1,  *$3, an); 
	delete $1;
	delete $3;
	$$ = new string(s1); 
}

| Term '/' Term {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::DIV);
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*$1, *$3, an); 
	delete $1;
	delete $3;
	$$ = new string(s1);
}

| Term '%' Term {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::MOD);
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*$1, *$3, an); 
	delete $1;
	delete $3;
	$$ = new string(s1);
}

| Term '*' Term {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::TIMES);
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*$1, *$3, an); 
	delete $1;
	delete $3;
	$$ = new string(s1);
}
| Term '-' Term {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::PLUS);	
	string neg1 = currentBD->new_name();
	arith_node* negn = newArithNode(arith_node::NEG);	
	negn->name = neg1;
	currentBD->new_node(*$3, "", negn);
	an->name = s1;
	currentBD->new_node(*$1, neg1, an); 
	
	
	Assert($1 != NULL && $3 != NULL, "THIS CAN't Happen");	
	delete $1;
	delete $3;
	$$ = new string(s1);
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
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::ARRACC);
	bool_node* yesChild = currentBD->get_node(*$3);
	bool_node* noChild = currentBD->get_node(*$5);
	an->multi_mother.push_back( noChild );
	an->multi_mother.push_back( yesChild );
	$$ = new string(s1);
	an->name = s1;
	currentBD->new_node(*$1, "", an); 
	if( $1 != NULL && $1 != $$){ delete $1; }
	if( $3 != NULL && $3 != $$){ delete $3; }
	if( $5 != NULL && $5 != $$){ delete $5; }		  					  
} 



varList: { /* Empty */  	$$ = new list<bool_node*>();	}
| Term varList{
//The signs are already in the stack by default. All I have to do is not remove them.
	if($1 != NULL){
		$2->push_back( currentBD->get_node(*$1) );
		delete $1;
	}else{
		$2->push_back( NULL );
	}
	$$ = $2;
}

IdentList: T_ident {
	$$ = new list<string*>();	
	$$->push_back( $1);
}
| T_ident IdentList{
	$$ = $2;
	$$->push_back( $1);
}

Term: Constant {
				 $$ = new string(currentBD->create_const($1));
				 }	 

| T_ident '[' T_vartype ']' '(' varList  ')' {
	
	list<bool_node*>* params = $6;
	if(params->size() == 0){
		if( $3 == INT){
			cout<<" INPUT IS INT "<<*$1<<endl;
			currentBD->create_inputs( 2 /*NINPUTS*/ , *$1); 
		}else{
			cout<<" INPUT IS BIT "<<*$1<<endl;
			currentBD->create_inputs(-1, *$1);
		}
		$$ = $1;
	}else{	
		string& fname = *$1;
		list<bool_node*>::reverse_iterator parit = params->rbegin();
		UFUN_node* ufun = new UFUN_node();
		ufun->name = fname;
		for( ; parit != params->rend(); ++parit){
			ufun->multi_mother.push_back((*parit));
		}
		
		if( $3 == INT){
			ufun->set_nbits( 2 /*NINPUTS*/  );
		}else{
	
			ufun->set_nbits( 1  );
		}
		
		
		
		string s1;
		{
			stringstream str;
			str<<fname<<"_"<<currentBD->size();
			s1 = str.str();
		}
		
		currentBD->new_node(NULL, NULL, ufun, s1);
		
		$$ = new string(s1);
		delete $1;
	}
	delete $6;
}

| '-' Term {
	string neg1 = currentBD->new_name();
	arith_node* negn = newArithNode(arith_node::NEG);
	negn->name = neg1;
	currentBD->new_node(*$2, "", negn);
	Assert($2 != NULL, "THIS CAN't Happen");	
	delete $2;
	$$ = new string(neg1);
}
| '!' Term { 
    /* Check the Boolean coefficient of the term, being either 0 (false) or 1 (true). */
    /* Generate an alternating NOT node, push a unit (true) coefficient. */
    string s = currentBD->new_name ();
    currentBD->new_node (*$2, "", bool_node::NOT, s);
    $$ = new string (s);
    delete $2;
}

| '(' Expression ')' { 
						$$ = $2; 
						}
| Ident { 
			if( !currentBD->has_alias(*$1) ){ 
				$$ = $1;
			}else{ 
				string alias(currentBD->get_alias(*$1)); 
				if(alias == ""){
					$$ = NULL;
				}else{
					$$ = new string( alias ); 
				}  
				delete $1;
			} 
		}
| '<' Ident '>' {
	currentBD->create_controls(-1, *$2);
	if( !currentBD->has_alias(*$2) ){ 
		$$ = $2;
	}else{ 
		Assert( false, "THIS SHOULD NEVER HAPPEN !!!!!!!!!!!!!!!!");
		string alias(currentBD->get_alias(*$2)); 
		$$ = new string( alias);
		delete $2;
	} 
}
| '<' Ident Constant '>' {
	int nctrls = $3;
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	int N=currentBD->create_controls(nctrls, *$2);
	$$ = $2;
}
| '<' Ident Constant '*' '>' {
	int N=currentBD->create_controls($3, *$2);
	$$ = $2;

}

ConstantExpr: ConstantTerm { $$ = $1; }
| ConstantExpr '+' ConstantTerm { $$ = $1 + $3; }
| ConstantExpr '-' ConstantTerm { $$ = $1 - $3; }

ConstantTerm: NegConstant { $$ = $1; }
| '(' ConstantTerm ')' { $$ = $2; }
| ConstantTerm '*' ConstantTerm { $$ = $1 * $3; } 
| ConstantTerm '/' ConstantTerm { Assert( $3 != 0, "You are attempting to divide by zero !!");
							      $$ = $1 / $3; } 
| ConstantTerm '%' ConstantTerm { Assert( $3 != 0, "You are attempting to mod by zero !!");
							      $$ = $1 % $3; }


NegConstant: Constant {  $$ = $1; }
| '-' Constant {  $$ = -$2; }

Constant: 
 T_int {  $$ = $1; }
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
