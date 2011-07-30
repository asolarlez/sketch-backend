#pragma once
#include "NodeVisitor.h"
#include <map>
#include <vector>

using namespace std;

class DeriveImplications :
	public NodeVisitor
{
	typedef map<pair<bool_node*, bool_node*>, vector<bool_node*> > lmap;
	lmap ineqmap;
	map<int, vector<pair<bool_node*, int> > > implications;
	pair<bool_node*, bool_node*> idx(bool_node& n);
	int getid(bool_node& n);
	bool isequiv(bool_node* n1, bool_node* n2);
public:
	bool checkImplications(bool_node* n, bool rev=false);
	vector<pair<bool_node*, int> >& getimps(bool_node* n, bool rev);
	DeriveImplications(void);
	~DeriveImplications(void);
	virtual void visit( LT_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ASSERT_node &node);	
	virtual void process(BooleanDAG& bdag);
};
