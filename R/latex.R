#' Signature page table function for latex signature page
#'
#' supply name and titles and output dataframe appropriate for pander::pandoc.table. Used in SOPs and QAPP.  sigs is list of sigs and title
#' @param sigs list of names and affiliation to be included in signatures
#' @param online should name be on line
#' @param vspace latex space between signature entries
#' @examples
#' sigs <- list(list(name = 'Joel Allen',
#'                   title = 'Principle Investigator',
#'                   online = 'test'),
#'              list(name = 'Michael Elovitz',
#'                   title = c('Branch Chief',
#'                             'Water Quality Management Branch')))
#' @export
sigFunc<-function(sigs, online = NULL, vspace = '0.75in'){
  # set \dateline length
  cat('\\ifcsdef{dateline}{}{\\newlength{\\dateline}}\n')
  cat('\\begin{minipage}{\\textwidth}\n')
  plyr::l_ply(sigs,
              .fun = function(x){
                lvar <- gsub('[^a-zA-Z0-9.]', '', x$name)
                cat(paste0('\\setlength\\dateline{0.7\\textwidth}',
                           '\\newlength{\\', lvar,
                           '}\\settowidth{\\', lvar,
                           '}{',x$name,'}'),
                    if(!is.null(vspace)){
                      paste0('\\vspace{', vspace, '}')
                    },
                    if(!is.null(x$online)){
                      paste0(x$online, '\\newline')
                    },
                    '\\rule{0.5\\textwidth}{0.5pt}\\hspace{0.2\\textwidth}\\rule{0.3\\textwidth}{0.5pt}',
                    paste0('\\advance\\dateline by -\\', lvar),
                    paste0(x$name, '\\hspace{\\dateline}Date\\newline'),
                    plyr::laply(x$title,
                                .fun = function(y){
                                  paste0(y, '\\newline')
                                }),
                    '  \n',
                    sep = '\n')
              })
  cat('\\end{minipage}')
}
