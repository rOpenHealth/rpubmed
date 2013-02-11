rpubmed
=======

Tools for extracting and processing records from Pubmed and Pubmed Central.
----------------------------------------------------------------------------

This project is still very much in development... Please contact me with any questions, suggestions or bug reports.  

I have built in support for searching and processing [MeSH headings](http://www.nlm.nih.gov/bsd/disted/meshtutorial/introduction/index.html), making this package of particular use for biomedical researchers conducting systematic reviews and meta-analyses. Two of these functions (`mesh_assoc_table` and `keyword_assoc_table`) produce association matrices which can be fed into graph packages such as `igraph` to visualise the associations between different search terms.




### Available functions:

* fetch - Tools for bulk downloading of Pubmed records
    - `fetch_in_chunks(ids, chunk_size = 500, delay = 0, ...)`
    - `pubmed_fetch(ids, file_format = "xml", as_r_object = TRUE, ...)`
* textsearch  - Tools for text-mining of abstracts and metadata from downloaded records
    - `get_articles_by_terms(corpus, term_list, where, case_sensitive = FALSE)`
    - `record_counts_by_year(corpus)`
* io - saving records to disk and printing summaries of abstract lists to file or sdout
    - `write_JSON_file(x, file)`
    - `write_record_list(articles, out_file = "", abstract_p = FALSE, markdown_p = FALSE, linestart = "* ")`
* locations - Geocoding functionality added for finding the coordinates of departments affiliated with Pubmed Articles.
    - `geocode_addresses(addresses, sleeper = 0.33, depth = 3)`
    - `get_article_location_data(abstracts)`
    - `geocode_address(address, depth = 3)`
* mesh - Tools for processing and exploring associations between MeSH headings and other keywords
    - `mesh_assoc_table(corpus)`
    - `keyword_assoc_table(corpus, keyword_list, ...)`
    - `get_mesh_headings(article)`
    - `mesh_heading_frequency(corpus)`
    






