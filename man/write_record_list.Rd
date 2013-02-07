\name{write_record_list}
\alias{write_record_list}
\title{Writes article title and citation data to file or stdout.  Can optionally also output abstracts and output as Markdown, with customisable line starts, e.g. for unordered lists}
\usage{
  write_record_list(articles, out_file = "",
    abstract_p = FALSE, markdown_p = FALSE,
    linestart = "* ")
}
\arguments{
  \item{articles}{A list of Pubmed Records e.g. as returned
  by fetch_in_chunks()}

  \item{out_file}{character file to write results to. Empty
  string returns to stdout}

  \item{abstract_p}{boolean Output the abstract?}

  \item{linestart}{Character string to add at the front of
  each line, controlling markdown output. Default is "* "}

  \item{markdown_p}{boolean Output as markdown?}
}
\value{
  NULL
}
\description{
  Writes article title and citation data to file or stdout.
  Can optionally also output abstracts and output as
  Markdown, with customisable line starts, e.g. for
  unordered lists
}

