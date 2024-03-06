server <- socketConnection(host = "localhost",
                           port = 8888,
                           server = TRUE,
                           blocking = TRUE)


con <- socketConnection("127.0.0.1", port = 9000)


library(socket)
server <- svSocket::server(port = 9000)
