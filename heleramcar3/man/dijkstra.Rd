\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (graph, init_node) 
{
    nodes <- unique(c(graph$v1, graph$v2))
    distances <- rep(Inf, length(nodes))
    distances[which(nodes == init_node)] <- 0
    visited < -rep(FALSE, length(nodes))
    while (TRUE) {
        current_node <- which.min(distances[!visited])
        if (is.infinite(distances[current_node])) 
            break
        visited[current_node] <- TRUE
        neighbors <- graph[graph$v1 == nodes[current_node], ]
        for (i in 1:nrow(neighbors)) {
            neighbors_index <- which(nodes == neighbors$v2[i])
            if (!visited[neighbors_index]) {
                new_distance <- distances[current_node] + neighbors$w[i]
                if (new_distance < distances[neighbors_index]) {
                  distances[neighbors_index] <- new_distance
                }
            }
        }
    }
    return(distances)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
