#pragma once
#include "StringHTable.h"

class BitSet {
	int size;
	unsigned buf[];
	BitSet(int sz) :size(sz) {
		memset(buf, 0, sz * sizeof(unsigned));
	}
public:

	void insert(int v) {
		int idx = v >> 5;
		int msk = 1 << (v & 0x1f);
		buf[idx] |= msk;
	}

	void print(ostream& os);

	void insert(BitSet* other) {
		if (other == NULL) { return; }
		Assert(size == other->size, "e;lkhy");
		unsigned* oth = other->buf;
		for (int i = 0; i<size; ++i) {
			buf[i] |= oth[i];
		}
	}
	friend BitSet* merge(Ostore<unsigned>& store, BitSet* a, BitSet* b);
	int next(int v) {
		++v;
		int msz = size << 5;
		while (v < msz) {
			int idx = v >> 5;
			int msk = 1 << (v & 0x1f);
			if ((buf[idx] & msk) != 0) {
				return v;
			}
			++v;
		}
		return -1;
	}
	/*
	Low level creator; should only be called by methods that know what they are doing.
	*/
	friend BitSet* mybitsetcreateLL(Ostore<unsigned>& store, int wsize);

	/*
	Public factory method to create a BitSet.
	*/
	friend BitSet* mybitsetcreate(Ostore<unsigned>& store, int sz);
};


inline BitSet* mybitsetcreateLL(Ostore<unsigned>& store, int wsize) {
	BitSet* rv = new(store.newObj(wsize + 1)) BitSet(wsize);
	return rv;
}

inline BitSet* mybitsetcreate(Ostore<unsigned>& store, int sz) {
	int wsize = sz > 0 ? (((sz - 1) >> 5) + 1) : 0;
	BitSet* rv = new(store.newObj(wsize + 1)) BitSet(wsize);
	return rv;
}

inline BitSet* merge(Ostore<unsigned>& store, BitSet* a, BitSet* b) {
	if (b == NULL) {
		return a;
	}
	if (a == NULL) {
		return b;
	}
	Assert(a->size == b->size, "Clekn");
	unsigned* ba = a->buf;
	unsigned* bb = b->buf;
	int sz = a->size;
	for (int i = 0; i<sz; ++i) {
		if (ba[i] != (ba[i] | bb[i])) {
			BitSet* rv = mybitsetcreateLL(store, sz);
			for (int j = 0; j<sz; ++j) {
				rv->buf[j] = ba[j] | bb[j];
			}
			return rv;
		}
	}
	return a;
}
