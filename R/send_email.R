
#' send_email.R
#' 
#' sends an HTML formatted email. Supports basic markdown formatting
#' and adds a default signature
#'
#' @param email_body message body in plain text or markdown. Will be converted to HTML
#' @param subject email subject
#' @param to list of recipients
#' @param from default is <rollie.parrish@providence.org>
#' @param sig append Rollie's signature block? default = TRUE
#' @param ... additional command

send_email <- function(email_body = NULL, 
         subject = NULL, 
         to = "rollie.parrish@providence.org",
         from = "rollie.parrish@providence.org",
         sig = TRUE, 
         ...) {
        
        if(sig) {signature <- "
        --<br />
        Rollie Parrish, RN, BSN<br />
        Manager - Quality & Analytics | PHC Performance Improvement<br />
        Providence Sacred Heart Medical Center & Providence Holy Family Hospital | Spokane, WA<br />
        509.474.6542<br />
        
        "}
        
        body <- 
                paste("<HTML>",
                      "<span style=\"font-family: Calibri, Helvetica, sans-serif; \">",
                      commonmark::markdown_html(email_body),
                      signature,
                      "</span>",
                      "</BODY></HTML>")
        
        
        #' send email
        result <- 
                mailR::send.mail(from = from,
                          to = to, 
                          bcc = c("rollie.parrish@providence.org"), 
                          subject = subject, 
                          inline = TRUE, 
                          html = TRUE, 
                          body = body,  
                          #body = "deleteme.html",  
                          #attach.files = c("./deleteme.pdf"), 
                          #file.names = c("Delete me.pdf"), # optional parameter
                          #file.descriptions = c("Please delete this file"), # optional parameter 
                          debug = FALSE,
                          smtp = list(host.name = "smtphost.providence.org"),
                          authenticate = FALSE,
                          send = TRUE, ...)
        }
