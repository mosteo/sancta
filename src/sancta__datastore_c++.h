#ifndef _sancta__datastore_
#define _sancta__datastore_

#include <string>

using namespace std;

namespace sancta {

    namespace datastore {

	void set_int (const string &store, const string &key, int val);
	void set_dbl (const string &store, const string &key, double val);
	void set_str (const string &store, const string &key, const string &val);

	int    get_int (const string &store, const string &key);
	double get_dbl (const string &store, const string &key);
	string get_str (const string &store, const string &key);

	bool contains (const string &store, const string &key);

    }

}

#endif
