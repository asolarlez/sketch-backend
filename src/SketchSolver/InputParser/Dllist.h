#ifndef DLISTNODE_H
#define DLISTNODE_H 1


class DllistNode;

class Dllist{
public:
	DllistNode* head;
	DllistNode* tail;
	Dllist(){ head = NULL; tail = NULL; }
	void append(DllistNode* n);
	void repOK();
};



class DllistNode{
public:
	Dllist* parent;

	DllistNode* prev;
	DllistNode* next;
	
	DllistNode(){
		prev = NULL;
		next = NULL;
		parent = NULL;
	}
	void remove(){
		if(parent != NULL){
			if(parent->head==this){
				parent->head = next;
			}
			if(parent->tail==this){
				parent->tail = prev;
			}
		}
		if(prev != NULL){
			prev->next = next;
		}
		if(next != NULL){
			next->prev = prev;			
		}
		prev = NULL;
		next = NULL;
	}
	void addBefore(DllistNode* n){
		if(prev==NULL){
			prev = n;
			n->next = this;
			Assert(parent->head == this, "mlkey;km4");
			parent->head = n;
		}else{
			n->prev = prev;
			prev->next = n;
			prev = n;
			n->next = this;
		}
		n->parent = parent;
	}

	virtual string lprint()const=0;

	virtual void print(){
		cout<<"N= "<<this->lprint()<<endl;
		/*
		ASSERT_node* an = dynamic_cast<ASSERT_node*>(this);
		if(an == NULL){
			UFUN_node* un = dynamic_cast<UFUN_node*>(this);
				if(un != NULL){
					cout<<"N="<<un->lprint()<<endl;
				}
		}else{
			cout<<"N="<<an->lprint()<<endl;
		}*/
		if(next != NULL){
			next->print();
		}
	}

	void add(DllistNode* n){
		if(next == NULL){
			next = n;
			n->prev = this;
			Assert(parent->tail == this, "What !?");
			parent->tail = n;
		}else{
			n->next = next;
			next->prev = n;
			next = n;
			n->prev = this;
		}
		
		n->parent = parent;
	}
	void add(Dllist* dl){
		while(dl->tail != NULL){
			DllistNode* tmp = dl->tail;
			tmp->remove();
			add(tmp);
		}
	}

	virtual ~DllistNode(){
		remove();
	}
};

inline void Dllist::append(DllistNode* n){
	if(tail == NULL){
		tail = n;
		head = n;
		n->parent = this;
	}else{
		tail->add(n);
		tail = n;
		Assert(tail->next == NULL, "This should be true");
	}		
}

inline void Dllist::repOK(){
	{
		DllistNode* tmp  = this->head;
		DllistNode* prev = NULL;
		while(tmp != NULL){
			Assert(tmp->next == NULL || tmp->next->prev == tmp, "BAD");
			prev = tmp;
			tmp = tmp->next;
		}
		Assert(prev == tail, "BAD");
	}
	{
		DllistNode* tmp  = this->tail;
		DllistNode* prev = NULL;
		while(tmp != NULL){
			Assert(tmp->prev == NULL || tmp->prev->next == tmp, "BAD");
			prev = tmp;
			tmp = tmp->prev;
		}
		Assert(prev == head, "BAD");
	}

}

#endif

