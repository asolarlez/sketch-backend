%{

using namespace std;

BooleanDAGCreator* currentBD;
stack<string> namestack;
vartype Gvartype;
string tupleName;

bool isModel;




#ifdef CONST
#undef CONST
#endif


#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
extern int yylex (YYSTYPE* yylval, yyscan_t yyscanner);

%}

%pure_parser
%parse-param {yyscan_t yyscanner}
%lex-param {yyscan_t yyscanner}


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
	bool_node* bnode;
  OutType* otype;
  vector<OutType*>* tVector;
  vector<string>* sVector;
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
%token T_rightAC
%token T_leftAC
%token T_rightTC
%token T_leftTC
%token T_leftAR
%token T_rightAR
%token T_arrow
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

%token T_Typedef
%token T_def
%token T_mdldef
%token T_Min
%token T_sp
%token T_assert
%token T_assume
%token T_hassert

%token T_equals
%token T_replace

%token T_eof

%type<intConst> Program
%type<strConst> Ident
%type<intConst> WorkStatement
%type<strConst> OptionalMsg
%type<bnode> Expression
%type<bnode> Term
%type<intConst> NegConstant
%type<intConst> Constant
%type<intConst> ConstantExpr
%type<intConst> ConstantTerm
%type<nList> varList
%type<sList> IdentList
%type<sList> TokenList
%type<bdag> AssertionExpr
%type<otype> TupleType
%type<tVector> TupleTypeList
%type<sVector> ParentsList



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

Program: Typedef MethodList T_eof{ solution.start(); int tmp= envt->doallpairs() ; solution.stop(); return tmp; }


MethodList: {}
| Method MethodList {}
| HLAssertion MethodList {}
| Replacement MethodList {}


InList: T_ident { 
    if(Gvartype == TUPLE){
        currentBD->create_inputs( -1, OutType::getTuple(tupleName), *$1);
    }
    else
    if( Gvartype == INT){
		currentBD->create_inputs( 2 /*NINPUTS*/, OutType::INT , *$1); 
	}else{
		if(Gvartype==FLOAT){
			currentBD->create_inputs(-1, OutType::FLOAT, *$1); 
		}else{
			currentBD->create_inputs(-1, OutType::BOOL, *$1); 
		}
	}	

}
| T_ident {
	if(Gvartype == TUPLE){
        currentBD->create_inputs( -1, OutType::getTuple(tupleName), *$1);
    }
    else
    if( Gvartype == INT){
		currentBD->create_inputs( 2 /*NINPUTS*/, OutType::INT , *$1); 
	}else{
		if(Gvartype==FLOAT){
			currentBD->create_inputs(-1, OutType::FLOAT, *$1); 
		}else{
			currentBD->create_inputs(-1, OutType::BOOL, *$1); 
		}
	}	
} InList

OutList: T_ident { 	 currentBD->create_outputs(-1, *$1); }
| T_ident OutList{
	
	currentBD->create_outputs(-1, *$1);
}


ParamDecl: T_vartype T_ident {  
	if( $1 == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/, OutType::INT , *$2); 
	}else{
		if($1 == FLOAT){
			currentBD->create_inputs(-1, OutType::FLOAT, *$2); 
		}else{
			currentBD->create_inputs(-1, OutType::BOOL, *$2); 
		}
	}	
	delete $2;
}
| T_ident T_ident{

    currentBD->create_inputs( -1 , OutType::getTuple(*$1), *$2);
       

    delete $2;
}
| T_ident T_ident '<' NegConstant '>' {

    currentBD->create_inputs( -1 , OutType::getTuple(*$1) , *$2, -1, $4);
       

    delete $2;
}
| '!' T_vartype T_ident {
 	 if( $2 == INT){

		 currentBD->create_outputs(2 /* NINPUTS */, *$3);
 	 }else{

	 	 currentBD->create_outputs(-1, *$3); 
 	 }
	 delete $3;
 }
 | '!' T_ident T_ident {

    currentBD->create_outputs(-1, *$3);
    delete $3;
 }
 |
 T_vartype '[''*' ConstantExpr']' T_ident {  
	if( $1 == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/, OutType::INT_ARR , *$6, $4); 
	}else{
		if($1 == FLOAT){
			currentBD->create_inputs(-1, OutType::FLOAT_ARR, *$6, $4); 
		}else{
			currentBD->create_inputs(-1, OutType::BOOL_ARR, *$6, $4); 
		}
	}	
	delete $6;
}
|
T_ident '[' '*' ConstantExpr ']' T_ident {
    currentBD->create_inputs(-1, OutType::getTuple(*$1), *$6, $4);
}

| '!' T_vartype '[''*' ConstantExpr']' T_ident {
 	 if( $2 == INT){
		 currentBD->create_outputs(2 /* NINPUTS */, *$7);
 	 }else{

	 	 currentBD->create_outputs(-1, *$7); 
 	 }
	 delete $7;
 }
 | '!' T_ident '[' '*' ConstantExpr ']' T_ident {
  currentBD->create_outputs(-1,*$7);
 }
| T_vartype '[' ConstantExpr ']'{Gvartype = $1; } InList 
| T_ident '[' ConstantExpr ']' {Gvartype = TUPLE; tupleName = *$1;} InList
| '!' T_vartype '[' ConstantExpr ']' OutList 
| '!' T_ident '[' ConstantExpr ']' OutList


ParamList: /*empty*/ 
| ParamDecl
| ParamDecl ',' ParamList 

Mhelp: T_mdldef {isModel=true; } | T_def {isModel=false; }

Method: Mhelp T_ident
{		modelBuilding.restart ();
		if(currentBD!= NULL){
			delete currentBD;
		}

		currentBD = envt->newFunction(*$2, isModel);

		delete $2;

}
'(' ParamList ')' '{' WorkBody '}' { 
	currentBD->finalize();
	modelBuilding.stop();
}


TupleType: T_vartype {
    if( $1 == INT){ $$ = OutType::INT;}
    if( $1 == BIT){ $$ = OutType::BOOL;}
    if( $1 == INT_ARR){ $$ = OutType::INT_ARR;}
    if( $1 == BIT_ARR){ $$ = OutType::BOOL_ARR;}
    if( $1 == FLOAT_ARR){ $$ = OutType::FLOAT_ARR;}
}
| T_vartype '[' '*' ConstantExpr ']' {
    if ($1 == INT) {$$ = OutType::INT_ARR;}
    if( $1 == BIT){ $$ = OutType::BOOL_ARR;}
    if( $1 == FLOAT){ $$ = OutType::FLOAT_ARR;}
}
| T_ident '[' '*' ConstantExpr ']' {
  $$ = ((Tuple*)OutType::getTuple(*$1))->arr;
}
| T_ident { 
    $$ = OutType::getTuple(*$1);
}

TupleTypeList: {/* Empty */  $$ = new vector<OutType*>(); }
|  TupleTypeList TupleType  {
    $1->push_back( $2 );
    $$ = $1;
}
| TupleTypeList T_vartype '[' Constant ']' {
    OutType* type;
    if ($2 == INT) {type = OutType::INT_ARR;}
    if( $2 == BIT){ type = OutType::BOOL_ARR;}
    if( $2 == FLOAT){type = OutType::FLOAT_ARR;}
    for (int i = 0; i < $4; i++ ) {
        $1->push_back (type );
    }
    $$ = $1;

}

TypeLine: T_ident '(' TupleTypeList ')'{
//add type
    OutType::makeTuple(*$1, *$3, -1);

}
| T_ident '(' Constant TupleTypeList ')' {
    OutType::makeTuple(*$1, *$4, $3);
}

TypeList: { /* Empty */ }
| TypeList TypeLine { }

Typedef: {/* Empty */}
|T_Typedef '{' TypeList '}'{ }

Replacement: T_replace T_ident '*' T_ident T_equals T_ident '(' NegConstant ')' ';' {
  envt->registerFunctionReplace(*$4, *$2, *$6, $8);
}


AssertionExpr: T_ident T_Sketches T_ident
{
	if(PARAMS->interactive){
		$$ = envt->prepareMiter(envt->getCopy(*$3),  envt->getCopy(*$1));
	}else{
		envt->addspskpair(*$3, *$1);
	}		
}

HLAssertion: T_assert {if(PARAMS->interactive){ solution.restart();} } AssertionExpr ';'
{
	if(PARAMS->interactive){
		int tt = envt->assertDAG($3, std::cout);
		envt->printControls("");
		solution.stop();
		cout<<"COMPLETED"<<endl;
		if(tt != 0){
			return tt;
		}
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
	currentBD->alias( *$1, $3);
	delete $1;
}							  
| '$' IdentList T_twoS varList '$''[' Expression ']' '=' Expression ';' {

	list<string*>* childs = $2;
	list<string*>::reverse_iterator it = childs->rbegin();
	
	list<bool_node*>* oldchilds = $4;
	list<bool_node*>::reverse_iterator oldit = oldchilds->rbegin();
	
	bool_node* rhs;
	rhs = $10;
	int bigN = childs->size();
	Assert( bigN == oldchilds->size(), "This can't happen");	

	for(int i=0; i<bigN; ++i, ++it, ++oldit){		
		ARRASS_node* an = dynamic_cast<ARRASS_node*>(newNode(bool_node::ARRASS));
		an->multi_mother.reserve(2);
		an->multi_mother.push_back(*oldit);			
		an->multi_mother.push_back(rhs);
		Assert( rhs != NULL, "AAARRRGH This shouldn't happen !!");
		Assert($7 != NULL, "1: THIS CAN'T HAPPEN!!");
		an->quant = i;		
		currentBD->alias( *(*it), currentBD->new_node($7,  NULL,  an) );
		delete *it;
	}
	delete childs;
	delete oldchilds;	
}

| T_OutIdent '=' Expression ';' {
	Assert(false, "UNREACHABLE");
	currentBD->create_outputs(2 /*NINPUTS*/, $3, *$1);
	delete $1;
}
| T_assert Expression ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */
    
    currentBD->new_node ($2, NULL, bool_node::ASSERT);
  }
} 
| T_assert Expression ':' T_string ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */
	if(!($2->type == bool_node::CONST && dynamic_cast<CONST_node*>($2)->getVal() == 1)){
		ASSERT_node* bn = dynamic_cast<ASSERT_node*>(newNode(bool_node::ASSERT));
		bn->setMsg(*$4);
		currentBD->new_node ($2, NULL, bn);
	}    
    delete $4;
  }
} 
| T_hassert Expression ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */
    
    ASSERT_node* bn = dynamic_cast<ASSERT_node*>(newNode(bool_node::ASSERT));
    bn->makeHardAssert();
    currentBD->new_node($2, NULL, bn);
  }
} 
| T_hassert Expression ':' T_string ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */
	if(!($2->type == bool_node::CONST && dynamic_cast<CONST_node*>($2)->getVal() == 1)){
		ASSERT_node* bn = dynamic_cast<ASSERT_node*>(newNode(bool_node::ASSERT));
		bn->setMsg(*$4);
    bn->makeHardAssert();
		currentBD->new_node ($2, NULL, bn);
	}    
    delete $4;
  }
}

| T_assume Expression OptionalMsg ';' {
  if ($2) {
    /* Asserting an expression, construct assert node. */
	if(!($2->type == bool_node::CONST && dynamic_cast<CONST_node*>($2)->getVal() == 1)){
		ASSERT_node* bn = dynamic_cast<ASSERT_node*>(newNode(bool_node::ASSERT));
		bn->makeAssume();
		if ($3) {
			bn->setMsg(*$3);
		}
		currentBD->new_node ($2, NULL, bn);
	}
	if ($3) {
		delete $3;
	}
  }
}

OptionalMsg: ':' T_string { $$ = $2; }
	   | { $$ = 0; }

Expression: Term { $$ = $1; }
| Term '&' Term {
	$$ = currentBD->new_node($1,  $3, bool_node::AND);	
}
| Term T_and Term{
	$$ = currentBD->new_node($1,  $3, bool_node::AND);
}
| Term '|' Term {
	$$ = currentBD->new_node($1,  $3, bool_node::OR);	
}
| Term T_or Term { 	
	$$ = currentBD->new_node($1,  $3, bool_node::OR);	
}
| Term '^' Term{	
	$$ = currentBD->new_node($1,  $3, bool_node::XOR);	
}
| Term T_neq Term{	
	bool_node* tmp = currentBD->new_node($1,  $3, bool_node::EQ);
	$$ = currentBD->new_node (tmp, NULL, bool_node::NOT);	
}
| Term T_eq Term { 			
	$$ = currentBD->new_node($1,  $3, bool_node::EQ);
}
| Term T_leftAR Expression T_rightAR {
	$$ = currentBD->new_node($3, $1, bool_node::ARR_R);
}
| Term '.' '[' NegConstant ']'{
   
	//TUPLE_R_node* tn = dynamic_cast<TUPLE_R_node*>();
    
	//tn->idx = $4;
	$$ = currentBD->new_node($1, $4);
	
}
| NegConstant '[' Expression ']' {
	$$ = currentBD->new_node($3, currentBD->create_const($1), bool_node::ARR_R);		
}
| T_ident '[''[' Expression T_arrow Expression  ']'']'{
	ARR_W_node* an = dynamic_cast<ARR_W_node*>(newNode(bool_node::ARR_W));
	an->multi_mother.push_back( currentBD->get_node(*$1) );
	an->multi_mother.push_back( $6 );
	$$ = currentBD->new_node($4, NULL, an);	
	delete $1;
}
| NegConstant '[''[' Expression T_arrow Expression  ']'']'{
	ARR_W_node* an = dynamic_cast<ARR_W_node*>(newNode(bool_node::ARR_W));
	an->multi_mother.push_back( currentBD->create_const($1) );
	an->multi_mother.push_back( $6 );
	$$ = currentBD->new_node($4, NULL, an);		
}
| '$' varList '$' '[' Expression ']' {
	int pushval = 0;
	arith_node* an = dynamic_cast<arith_node*>(newNode(bool_node::ARRACC));
	list<bool_node*>* childs = $2;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}		
	Assert($5 != NULL, "2: THIS CAN'T HAPPEN!!");	
	$$ = currentBD->new_node($5, NULL,  an);
	delete childs;	
}
| T_leftAC varList T_rightAC{
	arith_node* an = dynamic_cast<arith_node*>(newNode(bool_node::ARR_CREATE));
    

	list<bool_node*>* childs = $2;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}		
	$$ = currentBD->new_node(NULL, NULL, an); 
	delete childs;
}
| '[' T_ident ']' T_leftTC varList T_rightTC{

	arith_node* an = dynamic_cast<arith_node*>(newNode(bool_node::TUPLE_CREATE));

	list<bool_node*>* childs = $5;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}
    (dynamic_cast<TUPLE_CREATE_node*>(an))->setName(*$2);
	$$ = currentBD->new_node(NULL, NULL, an); 
	delete childs;
}
| T_twoS varList T_twoS {
	arith_node* an = dynamic_cast<arith_node*>(newNode(bool_node::ACTRL));
	list<bool_node*>* childs = $2;
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}		
	$$ = currentBD->new_node(NULL, NULL, an); 
	delete childs;
}

| Term '+' Term {
	$$ = currentBD->new_node($1,  $3, bool_node::PLUS); 	
}

| Term '/' Term {	
	$$ = currentBD->new_node($1,  $3, bool_node::DIV); 	
}

| Term '%' Term {	
	$$ = currentBD->new_node($1,  $3, bool_node::MOD); 	
}

| Term '*' Term {
	$$= currentBD->new_node($1,  $3, bool_node::TIMES);
}
| Term '-' Term {
	bool_node* neg1 = currentBD->new_node($3, NULL, bool_node::NEG);
	$$ = currentBD->new_node($1, neg1, bool_node::PLUS); 	
}
| Term '>' Term {
	
	$$ = currentBD->new_node($3, $1, bool_node::LT);     
}
| Term '<' Term {
	$$ = currentBD->new_node($1, $3, bool_node::LT);
}
| Term T_ge Term {
	bool_node* tmp = currentBD->new_node($1, $3, bool_node::LT);
	$$ = currentBD->new_node(tmp, NULL, bool_node::NOT);
}
| Term T_le Term {
	bool_node* tmp = currentBD->new_node($3, $1, bool_node::LT);
	$$ = currentBD->new_node(tmp, NULL, bool_node::NOT);
}
| Expression '?' Expression ':' Expression {
	arith_node* an = dynamic_cast<arith_node*>(newNode(bool_node::ARRACC));
	bool_node* yesChild =($3);
	bool_node* noChild = ($5);
	an->multi_mother.push_back( noChild );
	an->multi_mother.push_back( yesChild );	
	$$ = currentBD->new_node($1, NULL, an); 	
} 



varList: { /* Empty */  	$$ = new list<bool_node*>();	}
| Term varList{

//The signs are already in the stack by default. All I have to do is not remove them.
	if($1 != NULL){
		$2->push_back( $1 );
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
	$$ = currentBD->create_const($1);
}	 
| T_dbl {
	$$ = currentBD->create_const($1);
}

| T_ident '[' '*' T_ident ']' '(' varList ')' '(' Expression ')' '[' T_ident ',' Constant ']' {
    
	list<bool_node*>* params = $7;
	if(false && params->size() == 0){

        $$ = currentBD->create_inputs(-1,OutType::getTuple(*$4), *$1);

		delete $1;
	}else{	
		string& fname = *$1;
		list<bool_node*>::reverse_iterator parit = params->rbegin();
		UFUN_node* ufun = new UFUN_node(fname);
		ufun->outname = *$13;
		int fgid = $15;
		ufun->fgid = fgid;
		bool_node* pCond;	
		for( ; parit != params->rend(); ++parit){
            ufun->multi_mother.push_back((*parit));
        }
        pCond = $10;


        ufun->set_nbits( 0 );
        ufun->set_tupleName(*$4);
		
		
		//ufun->name = (currentBD->new_name(fname));
		$$ = currentBD->new_node(pCond, NULL, ufun);

        delete $1;
		delete $13;
	}
	delete $7;

}
| T_ident '[' T_vartype ']' '(' varList  ')''(' Expression ')' '[' T_ident ',' Constant ']' {
	
	list<bool_node*>* params = $6;
	if(false && params->size() == 0){
		if( $3 == INT){
			$$ = currentBD->create_inputs( 2 /*NINPUTS*/, OutType::INT , *$1); 
		}else{
			if($3==FLOAT){
				$$ = currentBD->create_inputs(-1,OutType::FLOAT, *$1);
			}else{
				$$ = currentBD->create_inputs(-1,OutType::BOOL, *$1);
			}
		}
		delete $1;
	}else{	
		string& fname = *$1;
		list<bool_node*>::reverse_iterator parit = params->rbegin();
		UFUN_node* ufun = new UFUN_node(fname);
		ufun->outname = *$12;
		int fgid = $14;
		ufun->fgid = fgid;	
		bool_node* pCond;	

        for( ; parit != params->rend(); ++parit){
            ufun->multi_mother.push_back((*parit));
        }
        pCond = $9;

		
		if( $3 == INT || $3==INT_ARR){
			ufun->set_nbits( 2 /*NINPUTS*/  );
		}else{	
			ufun->set_nbits( 1  );
		}
		if($3 == INT_ARR || $3==BIT_ARR){
			ufun->makeArr();
		}
		
		//ufun->name = (currentBD->new_name(fname));
		$$ = currentBD->new_node(pCond, NULL, ufun);

		
		delete $1;
		delete $12;
	}
	delete $6;
}

| '-' Term {		
		$$ = currentBD->new_node($2, NULL, bool_node::NEG);				
}
| '!' Term { 
	$$ = currentBD->new_node($2, NULL, bool_node::NOT);		    
}

| '(' Expression ')' { 
						$$ = $2; 
						}
| Ident { 			
			$$ = currentBD->get_node(*$1);
			delete $1;				
			 
		}
| '<' Ident '>' {		
	$$ = currentBD->create_controls(-1, *$2);
	delete $2;
}
| '<' Ident Constant '>' {
	int nctrls = $3;
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	$$ = currentBD->create_controls(nctrls, *$2);
	delete $2;
}
| '<' Ident Constant '*' '>' {
	$$ = currentBD->create_controls($3, *$2);
	delete $2;

}
| '<' Ident '+' '>' {
	$$ = currentBD->create_controls(-1, *$2, false, true);
	delete $2;
}
| '<' Ident Constant '+' '>' {
	$$ = currentBD->create_controls($3, *$2, false, true);
	delete $2;
}
| T_Min '<' Ident '>' {		
	$$ = currentBD->create_controls(-1, *$3, true);
	delete $3;
}
| T_Min '<' Ident Constant '>' {
	int nctrls = $4;
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	$$ = currentBD->create_controls(nctrls, *$3, true);
	delete $3;
}
| T_Min '<' Ident Constant '*' '>' {
	$$ = currentBD->create_controls($4, *$3, true);
	delete $3;

}
| T_sp Constant '$' ParentsList '$' '<' Ident '>' {
	$$ = currentBD->create_controls(-1, *$7, false, false, true, $2);
  ((CTRL_node*) $$)->setParents(*$4);
	delete $7;
}
| T_sp Constant '$' ParentsList '$' '<' Ident Constant '>' {
	int nctrls = $8;
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	$$ = currentBD->create_controls(nctrls, *$7, false, false, true, $2);
  ((CTRL_node*) $$)->setParents(*$4);
	delete $7;
}
| T_sp Constant '$' ParentsList '$' '<' Ident Constant '*' '>' {
	$$ = currentBD->create_controls($8, *$7, false, false, true, $2);
  ((CTRL_node*) $$)->setParents(*$4);
	delete $7;

}

ParentsList: { /* Empty */  	$$ = new vector<string>();	}
| ParentsList Ident {
  $1->push_back(*$2);
	$$ = $1;
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

void yyerror( void* yyscanner, const char* c){
	Assert(false, (char *)c); 
}


int isatty(int i){



return 1;
}
