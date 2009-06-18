%{

using namespace std;

BooleanDAGCreator* currentBD;
stack<string> namestack;
vartype Gvartype;

string *comparisson (string *p1, string *p2, bool_node::Type atype)
{
    Assert (p1 || p2, "Can't have both comparisson's children NULL");
   
    string s1 = currentBD->new_name();
    
    currentBD->new_node((p1 ? *p1 : ""), 
			(p2 ? *p2 : ""), atype, s1); 
    if (p1)
	delete p1;
    if (p2)
	delete p2;
    return new string(s1); 
}





#define YYLEX_PARAM yyscanner
#define YYPARSE_PARAM yyscanner
#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
extern int yylex (YYSTYPE* yylval, yyscan_t yyscanner);

%}

%pure_parser

%union {
	int intConst;
	bool boolConst;
	std::string* strConst;
	double doubleConst;		
	std::list<int>* iList;
	list<bool_node*>* nList;
	list<string*>* sList;
	vartype variableType;
	BooleanDAG* bdag;
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


%token T_Native
%token T_NativeMethod
%token T_Sketches
%token T_new
%token T_add
%token T_Init


%token T_def
%token T_assert

%token T_eof

%type<intConst> Program
%type<strConst> Ident
%type<intConst> WorkStatement
%type<strConst> Expression
%type<strConst> Term
%type<intConst> NegConstant
%type<intConst> Constant
%type<intConst> ConstantExpr
%type<intConst> ConstantTerm
%type<nList> varList
%type<sList> IdentList
%type<sList> TokenList
%type<bdag> AssertionExpr



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

Program: MethodList T_eof{  $$=0; return 0;}


MethodList: {}
| Method MethodList {}
| HLAssertion MethodList {}


InList: T_ident {  

    if( Gvartype == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *$1); 
	}else{

		currentBD->create_inputs(-1, *$1); 
	}	

}
| T_ident {
	
    if( Gvartype == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *$1); 
	}else{

		currentBD->create_inputs(-1, *$1); 
	}	
} InList

OutList: T_ident { 	 currentBD->create_outputs(-1, *$1); }
| T_ident OutList{
	
	currentBD->create_outputs(-1, *$1);
}


ParamDecl: T_vartype T_ident {  
	if( $1 == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *$2); 
	}else{

		currentBD->create_inputs(-1, *$2); 
	}	
}
| '!' T_vartype T_ident {
 	 if( $2 == INT){

		 currentBD->create_outputs(NINPUTS, *$3);
 	 }else{

	 	 currentBD->create_outputs(-1, *$3); 
 	 }
 }
| T_vartype {
Gvartype = $1;

 } '[' ConstantExpr ']' InList 
| '!' T_vartype '[' ConstantExpr ']' OutList 


ParamList: ParamDecl 
| ParamDecl ',' ParamList 


Method: T_def T_ident
{		modelBuilding.restart ();
		if(currentBD!= NULL){
			delete currentBD;
		}
		currentBD = envt->newFunction(*$2);
		delete $2;
}
'(' ParamList ')' '{' WorkBody '}' { 
	currentBD->finalize();
	modelBuilding.stop();
}



AssertionExpr: T_ident T_Sketches T_ident 
{
	$$ = envt->prepareMiter(envt->getCopy(*$3),  envt->getCopy(*$1));
}

HLAssertion: T_assert {solution.start();} AssertionExpr ';'
{
	int tt = envt->assertDAG($3, std::cout);
	envt->printControls("");
	solution.stop();
	cout<<"COMPLETED"<<endl;
	if(tt != 0){
		return tt;
	}
}
| T_ident '(' TokenList ')' ';'
{
	int tt = envt->runCommand(*$1, *$3);
	delete $1;
	delete $3;
	if(tt >= 0){
		return tt;
	}
}

TokenList:  {
	$$ = new list<string*>();	
}
| T_ident TokenList{
	$$ = $2;
	$$->push_back( $1);
}
| T_string TokenList{
	$$ = $2;
	$$->push_back( $1);
}


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
		string s1( currentBD->new_name() );
		ARRASS_node* an = dynamic_cast<ARRASS_node*>(newArithNode(arith_node::ARRASS));
		an->multi_mother.reserve(2);
		an->multi_mother.push_back(*oldit);			
		an->multi_mother.push_back(rhs);
		an->name = s1;
		Assert( rhs != NULL, "AAARRRGH This shouldn't happen !!");
		Assert($7 != NULL, "1: THIS CAN'T HAPPEN!!");
		an->quant = i;
		currentBD->new_node(*$7,  "",  an);
		currentBD->alias( *(*it), s1);
		delete *it;
	}
	delete childs;
	delete oldchilds;
	delete $7;
	delete $10;
}
| T_OutIdent '=' Expression ';' {
	currentBD->create_outputs(NINPUTS, currentBD->get_node(*$3), *$1);
	delete $3;
	delete $1;
}
| T_assert Expression ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */

    string s = currentBD->new_name ();
    currentBD->new_node (*$2, "", bool_node::ASSERT, s);

    delete $2;
  }
} 
| T_assert Expression ':' T_string ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */

    ASSERT_node* bn = dynamic_cast<ASSERT_node*>(newBoolNode(bool_node::ASSERT));
    bn->setMsg(*$4);
    currentBD->new_node (*$2, "", bn);
    

    delete $2;
    delete $4;
  }
} 


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
	string* tmp = comparisson($1, $3, bool_node::EQ);
    string s = currentBD->new_name ();
    currentBD->new_node (*tmp, "", bool_node::NOT, s);
    delete tmp;
    $$ = new string(s);
}
| Term T_eq Term { 			
	$$ = comparisson($1, $3, bool_node::EQ);
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
	currentBD->new_node(*$1,  *$3, bool_node::PLUS, s1); 
	delete $1;
	delete $3;
	$$ = new string(s1); 
}

| Term '/' Term {
	string s1 = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::DIV, s1); 
	delete $1;
	delete $3;
	$$ = new string(s1); 
}

| Term '%' Term {
	string s1 = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::MOD, s1); 
	delete $1;
	delete $3;
	$$ = new string(s1); 
}

| Term '*' Term {
	string s1 = currentBD->new_name();
	currentBD->new_node(*$1,  *$3, bool_node::TIMES, s1); 
	delete $1;
	delete $3;
	$$ = new string(s1); 
}
| Term '-' Term {
	string neg1 = currentBD->new_name();
	string s1 = currentBD->new_name();
	currentBD->new_node(*$3, "", bool_node::NEG, neg1);
	currentBD->new_node(*$1, neg1, bool_node::PLUS, s1); 
	delete $1;
	delete $3;
	$$ = new string(s1);
}
| Term '>' Term {
	$$ = comparisson($1, $3, bool_node::GT);
}
| Term '<' Term {
	$$ = comparisson($1, $3, bool_node::LT);
}
| Term T_ge Term {
	$$ = comparisson($1, $3, bool_node::GE);
}
| Term T_le Term {
	$$ = comparisson($1, $3, bool_node::LE);
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
	delete $1; 
	delete $3; 
	delete $5; 		  					  
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

| T_ident '[' T_vartype ']' '(' varList  ')''(' Expression ')' {
	
	list<bool_node*>* params = $6;
	if(false && params->size() == 0){
		if( $3 == INT){
			currentBD->create_inputs( 2 /*NINPUTS*/ , *$1); 
		}else{

			currentBD->create_inputs(-1, *$1);
		}
		$$ = $1;
	}else{	
		string& fname = *$1;
		list<bool_node*>::reverse_iterator parit = params->rbegin();
		UFUN_node* ufun = new UFUN_node(fname);
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
			s1 = currentBD->new_name(fname);
		}
		ufun->name = s1;
		currentBD->new_node(*$9, "", ufun);
		
		$$ = new string(s1);
		delete $1;
	}
	delete $6;
	delete $9;
}

| '-' Term {
	string neg1 = currentBD->new_name();	
	currentBD->new_node(*$2, "", bool_node::NEG, neg1);	
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
				Assert( alias != "", "You need to have an alias for "<<*$1);				
				$$ = new string( alias ); 				  
				delete $1;
			} 
		}
| '<' Ident '>' {
	currentBD->create_controls(-1, *$2);
	Assert( !currentBD->has_alias(*$2), "THIS SHOULD NEVER HAPPEN !!!!!!!!!!!!!!!!");	
	$$ = $2;	
}
| '<' Ident Constant '>' {
	int nctrls = $3;
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	currentBD->create_controls(nctrls, *$2);
	$$ = $2;
}
| '<' Ident Constant '*' '>' {
	currentBD->create_controls($3, *$2);
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
