#include "sancta__datastore_c++.h"

namespace sancta {

    namespace datastore {

	extern "C" {
	    void sancta__datastore__set_int
	      (const char *, const char *, int);
	    void sancta__datastore__set_double
	      (const char *, const char *, double);
	    void sancta__datastore__set_string
	      (const char *, const char *, const char *);

	    int sancta__datastore__get_int
	      (const char *, const char *);
	    double sancta__datastore__get_double
	      (const char *, const char *);
	    void sancta__datastore__get_string
	      (const char *, const char *, int max_len, char *);

	    int sancta__datastore__contains (const char *, const char *);
	}

	void set_int (const string &store, const string &key, int val)
	{
	    sancta__datastore__set_int (store.c_str(), key.c_str(), val);
	};

	void set_dbl (const string &store, const string &key, double val)
	{
	    sancta__datastore__set_double (store.c_str(), key.c_str(), val);
	};

	void set_str (const string &store, const string &key, const string &val)
	{
	    sancta__datastore__set_string
	      (store.c_str(), key.c_str(), val.c_str());
	};

	int    get_int (const string &store, const string &key)
	{
	    return sancta__datastore__get_int (store.c_str(), key.c_str());
	};

	double get_dbl (const string &store, const string &key)
	{
	    return sancta__datastore__get_double (store.c_str(), key.c_str());
	}

	string get_str (const string &store, const string &key)
	{
	    char buffer[257];
	    sancta__datastore__get_string
	      (store.c_str(), key.c_str(), sizeof(buffer), buffer);
	    return string (buffer);
	}


	bool contains (const string &store, const string &key) {
	    return
	       sancta__datastore__contains (store.c_str(), key.c_str()) != 0;
	}

    }

}
