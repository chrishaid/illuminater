% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/db_connections.R
\name{connect_illuminate}
\alias{connect_illuminate}
\title{connect to illuminat database or warehouse}
\usage{
connect_illuminate(server, database, port, user, password,
  db_type = "warehouse")
}
\arguments{
\item{server}{the IP address of the data base server}

\item{database}{the name of the database to be connected to}

\item{port}{the port number on which the database server listens}

\item{user}{a user name}

\item{password}{the user's password}

\item{db_type}{one of either "warehouse" or "direct".  "Warehouse" connects to
a SQL server database.  "Direct" connects to site specific PostgreSQL instance
maintained by Illuminate. You'll need to get our computer's (i.e. the machine
running using this R package) IP adress whilelisted by Illuminate.  One benefit of
the "warehouse" setting is that you can connect other databases (say, a PowerSchool
mirror).}
}
\description{
Simple wrapper function that establishes a conect to a database
that is suitable for using with \code{\link[dplyr]{dplyr}}.
}

