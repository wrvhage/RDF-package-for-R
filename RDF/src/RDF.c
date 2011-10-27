#include <stdlib.h>
#include <raptor2.h>
#include <Rdefines.h>

/*
 * RDF -> R (reading)
 */

typedef struct ns {
    int cnt;
    char **prefix;
    char **uri;
} ns;

typedef struct cache {
    ns *ns;
    int cnt;
    char **s;
    char **p;
    char **o;
} cache;

static void append_cache_data(cache * cache_data, char *s, char *p, char *o)
{
    cache_data->s = (char **) realloc(cache_data->s, (cache_data->cnt + 1) * sizeof(char *));
    cache_data->p = (char **) realloc(cache_data->p, (cache_data->cnt + 1) * sizeof(char *));
    cache_data->o = (char **) realloc(cache_data->o, (cache_data->cnt + 1) * sizeof(char *));
    cache_data->s[cache_data->cnt] = strdup(s);
    cache_data->p[cache_data->cnt] = strdup(p);
    cache_data->o[cache_data->cnt] = strdup(o);
    cache_data->cnt++;
}

char *term_to_string(raptor_term * term)
{
	return (char*)raptor_term_to_string(term);
}

static void cache_triple(void *user_data, raptor_statement * triple)
{
    append_cache_data((cache *) user_data,
		      term_to_string(triple->subject),
		      term_to_string(triple->predicate),
		      term_to_string(triple->object));
}

static void declare_namespace(ns * namespaces, raptor_namespace * nspace)
{
    const char *prefix = (const char *) raptor_namespace_get_prefix(nspace);
    raptor_uri *uri = raptor_namespace_get_uri(nspace);
    if (uri != NULL) {
		namespaces->prefix = (char **) realloc(namespaces->prefix, (namespaces->cnt + 1) * sizeof(char *));
		namespaces->uri = (char **) realloc(namespaces->uri, (namespaces->cnt + 1) * sizeof(char *));
		namespaces->prefix[namespaces->cnt] = (prefix != NULL) ? strdup(prefix) : strdup("");
		char *uri_string = (char *) raptor_uri_to_string(uri);
		int len = strlen(uri_string);
		char *enveloped_string = (char*) malloc((len + 3) * sizeof(char *));
		sprintf(enveloped_string,"<%s>", uri_string);
		enveloped_string[len+2] = '\0';
		namespaces->uri[namespaces->cnt] = enveloped_string;
		free(uri_string);
		namespaces->cnt++;
    }
}

SEXP rdf_load(SEXP arg)
{
    SEXP rv, s, p, o, n;
    int i;
	const char* source = CHAR(STRING_ELT(arg, 0));
	unsigned char *uri_str = NULL;
    raptor_world *world = NULL;
    raptor_parser *rdf_parser = NULL;
    raptor_uri *uri, *base_uri;
    cache *cache_data = (cache *) calloc(1, (size_t) sizeof(cache));
    ns *namespaces = (ns *) calloc(1, (size_t) sizeof(ns));
    cache_data->ns = namespaces;

    world = raptor_new_world();

	if (memcmp(source,"http",4 * sizeof(char)) == 0) {
	    uri = raptor_new_uri(world, (unsigned const char*)source);
	} else {
		uri_str = raptor_uri_filename_to_uri_string(source);
		uri = raptor_new_uri(world, uri_str);
	}
    base_uri = raptor_uri_copy(uri);

    rdf_parser = raptor_new_parser(world, "guess");
    raptor_parser_set_statement_handler(rdf_parser, (void *) cache_data, cache_triple);
    raptor_parser_set_namespace_handler(rdf_parser, (void *) namespaces, (void *) declare_namespace);

    raptor_parser_parse_uri(rdf_parser, uri, base_uri);
    raptor_free_parser(rdf_parser);

    raptor_free_uri(base_uri);
    raptor_free_uri(uri);

    raptor_free_world(world);
	free(uri_str);

    PROTECT(rv = NEW_LIST(4));
    PROTECT(s = NEW_CHARACTER(cache_data->cnt));
    PROTECT(p = NEW_CHARACTER(cache_data->cnt));
    PROTECT(o = NEW_CHARACTER(cache_data->cnt));
    PROTECT(n = NEW_CHARACTER(2 * namespaces->cnt));
    SET_VECTOR_ELT(rv, 0, s);
    SET_VECTOR_ELT(rv, 1, p);
    SET_VECTOR_ELT(rv, 2, o);
    SET_VECTOR_ELT(rv, 3, n);
    for (i = 0; i < cache_data->cnt; i++) {
		SET_STRING_ELT(s, i, mkChar(cache_data->s[i]));
		SET_STRING_ELT(p, i, mkChar(cache_data->p[i]));
		SET_STRING_ELT(o, i, mkChar(cache_data->o[i]));
		free(cache_data->s[i]);
		free(cache_data->p[i]);
		free(cache_data->o[i]);
    }
    for (i = 0; i < namespaces->cnt; i++) {
		SET_STRING_ELT(n, 2 * i + 0, mkChar(namespaces->prefix[i]));
		free(namespaces->prefix[i]);
		SET_STRING_ELT(n, 2 * i + 1, mkChar(namespaces->uri[i]));
		free(namespaces->uri[i]);
    }
    UNPROTECT(5);
    free(namespaces);
    free(cache_data);
    return rv;
}


/*
 * R -> RDF (writing)
 */
static raptor_serializer *rdf_serializer;

SEXP getListElement(SEXP list, const char *str)
{
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++) {
		if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
		    elmt = VECTOR_ELT(list, i);
		    break;
		}
    }
    return elmt;
}

raptor_term *parse_subject_predicate_string(raptor_world *world, const char *uri_string) {
	raptor_term *term = NULL;
	int len = strlen(uri_string);
	if (uri_string[0] == '<') { // parsing a URI
		char *uri = (char*)malloc((len - 1)*sizeof(char));
		sprintf(uri, "%.*s", len - 2, uri_string + 1);
		uri[len - 2] = '\0';
		term = raptor_new_term_from_counted_uri_string(world, (const unsigned char*)uri, len - 2);
		free(uri);
	} else if (uri_string[0] == '_') { // parsing a blank node
		char *blank = (char*)malloc((len - 1)*sizeof(char));
		sprintf(blank, "%.*s", len - 2, uri_string + 2);
		blank[len - 2] = '\0';
		term = raptor_new_term_from_counted_blank(world, (const unsigned char*)blank, len - 2);
		free(blank);
	} else {
		fprintf(stderr,"unable to parse URI %s, missing < > ?\n",uri_string);
	}
	return term;
}

raptor_term *parse_object_string(raptor_world *world, const char *object) {
	raptor_term *object_term = NULL;
	int len = strlen(object);
	if (object[0] == '<') { // parsing a URI
		char *uri = (char*)malloc((len - 1)*sizeof(char));
		sprintf(uri, "%.*s", len - 2, object + 1);
		uri[len - 2] = '\0';
		object_term = raptor_new_term_from_counted_uri_string(world, (const unsigned char*)uri, len - 2);
		free(uri);
	} else if (object[0] == '_') { // parsing a blank node
		char *blank = (char*)malloc((len - 1)*sizeof(char));
		sprintf(blank, "%.*s", len - 2, object + 2);
		blank[len - 2] = '\0';
		object_term = raptor_new_term_from_counted_blank(world, (const unsigned char*)blank, len - 2);
		free(blank);
	} else { // parsing a literal
		char *lit = NULL, *lang = NULL;
		int lit_len = 0, type_lang_len = 0;
		raptor_uri *datatype = NULL;
		char *type_lang = strrchr(object, '@');
		if (type_lang != NULL && 
		    *(type_lang - 1) == '\"') { // language literal
		    lit_len = ((type_lang - object) - 2);
			type_lang_len = ((object + len) - (type_lang + 1));
			lit = (char*)malloc((lit_len + 1) * sizeof(char));
			lang = (char*)malloc((type_lang_len + 1) * sizeof(char));
			sprintf(lang, "%.*s", type_lang_len, type_lang + 1);
			lang[type_lang_len] = '\0';
			sprintf(lit, "%.*s", lit_len, object + 1);
			lit[lit_len] = '\0';
		} else if ((type_lang = strrchr(object, '^')) != NULL &&
		           *(type_lang - 1) == '^' &&
		           *(type_lang - 2) == '\"') { // typed literal
			char *type;
			lit_len = ((len - (type_lang - object)) - 3);
		    type_lang_len = (((object + len) - (type_lang + 1)) - 2);
			lit = (char*)malloc((lit_len + 1) * sizeof(char));
			type = (char*)malloc((type_lang_len + 1) * sizeof(char));
			sprintf(type, "%.*s", type_lang_len, type_lang + 2);
			lang[type_lang_len] = '\0';
			sprintf(lit, "%.*s", lit_len, object + 1);
			lit[lit_len] = '\0';
		} else if (object[0] == '\"' && object[len - 1] == '\"') { // regular literal
			lit_len = len - 2;
			lit = (char*)malloc((lit_len + 1) * sizeof(char));
			sprintf(lit, "%.*s", lit_len, object + 1);
			lit[lit_len] = '\0';
		} else {
			fprintf(stderr,"unable to parse literal %s, missing \" \" or malformed language or type tag?\n",object);
			return NULL;
		}
		object_term = raptor_new_term_from_counted_literal(world, (const unsigned char*)lit, (size_t)lit_len, datatype, (const unsigned char*)lang, (size_t)type_lang_len);
		if (lit != NULL) free(lit);
		if (lang != NULL) free(lang);
	}
	return object_term;
}

void clean_up(raptor_term *base_uri_term, raptor_uri *base_uri,raptor_serializer *rdf_serializer,raptor_world *world) {
	if (base_uri_term != NULL) raptor_free_term(base_uri_term);
	if (base_uri != NULL) raptor_free_uri(base_uri);
	if (rdf_serializer != NULL) raptor_free_serializer(rdf_serializer);
	if (world) raptor_free_world(world);
}


SEXP rdf_save(SEXP data, SEXP target, SEXP format, SEXP spo, SEXP namespaces)
{
	SEXP triples, subject, predicate, object;
    const char *format_string = (isNull(format) ? "rdfxml" : CHAR(STRING_ELT(format, 0))),
	           *filename = CHAR(STRING_ELT(target,0));
	raptor_world *world = NULL;
	raptor_uri *base_uri = NULL;
	raptor_term *base_uri_term = NULL;
    int i;

	subject = Rf_asCharacterFactor(getListElement(data, CHAR(STRING_ELT(spo,0))));
	if (length(subject) == 0) {
		printf("failed to interpret subject %s\n",CHAR(STRING_ELT(spo,0)));
		clean_up(base_uri_term,base_uri,rdf_serializer,world);
		return R_NilValue;
	}
	predicate = Rf_asCharacterFactor(getListElement(data, CHAR(STRING_ELT(spo,1))));
	if (length(predicate) == 0) {
		printf("failed to interpret predicate %s\n",CHAR(STRING_ELT(spo,1)));
		clean_up(base_uri_term,base_uri,rdf_serializer,world);
		return R_NilValue;
	}
	object = Rf_asCharacterFactor(getListElement(data, CHAR(STRING_ELT(spo,2))));
	if (length(object) == 0) {
		printf("failed to interpret object %s\n",CHAR(STRING_ELT(spo,2)));
		clean_up(base_uri_term,base_uri,rdf_serializer,world);
		return R_NilValue;
	}
	
	world = raptor_new_world();
	base_uri = raptor_new_uri(world, (const unsigned char*)filename);
	base_uri_term = raptor_new_term_from_uri(world, base_uri);
	rdf_serializer = raptor_new_serializer(world, format_string);
	raptor_serializer_start_to_filename(rdf_serializer, filename);
	for (i = 0; i < length(namespaces); i+=2) {
		const char *prefix = CHAR(STRING_ELT(namespaces, i));
		const char *enveloped_uri = CHAR(STRING_ELT(namespaces, i+1));
		raptor_term *term =  parse_subject_predicate_string(world,enveloped_uri);
		if (term == NULL) {
			printf("failed to parse namespace %s\n",enveloped_uri);
			clean_up(base_uri_term,base_uri,rdf_serializer,world);
			return R_NilValue;
		}
		raptor_serializer_set_namespace(rdf_serializer, (raptor_uri*)(term->value.uri), (strlen(prefix) > 0) ? (const unsigned char*)prefix : NULL);
		raptor_free_term(term);
	}
    for (i = 0; i < length(subject); i++) {
	    const char *s = CHAR(STRING_ELT(subject, i)),
	               *p = CHAR(STRING_ELT(predicate, i)),
		           *o = CHAR(STRING_ELT(object, i));
		raptor_term *st = parse_subject_predicate_string(world, s),
		            *pt = parse_subject_predicate_string(world, p),
		            *ot = parse_object_string(world, o);
		if (st == NULL) {
			printf("failed to parse term %s\n",s);
			clean_up(base_uri_term,base_uri,rdf_serializer,world);
			return R_NilValue;
		}
		if (pt == NULL) {
			printf("failed to parse term %s\n",p);
			clean_up(base_uri_term,base_uri,rdf_serializer,world);
			return R_NilValue;
		}
		if (ot == NULL) {
			printf("failed to parse term %s\n",o);
			clean_up(base_uri_term,base_uri,rdf_serializer,world);
			return R_NilValue;
		}
		raptor_statement *triple = raptor_new_statement_from_nodes(world, st, pt, ot, base_uri_term);
		raptor_serializer_serialize_statement(rdf_serializer, triple);
    }
	raptor_serializer_serialize_end(rdf_serializer);
	
	clean_up(base_uri_term,base_uri,rdf_serializer,world);
    return R_NilValue;
}

