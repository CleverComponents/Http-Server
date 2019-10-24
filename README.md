# Multithreaded HTTP Server in Delphi

<img align="left" src="https://www.clevercomponents.com/images/HttpServer-2-250.jpg" />

This article represents the fully functional multithreaded multi connection HTTP server that works asynchronously in a thread pool, accepts the GET, POST, PUT and any other HTTP requests, and sends the corresponding user-defined responses.

We have added the SSL / TLS support to the TclHttpServer component, implemented server-side certificate management, and updated the HttpServerDemo project. Now, this demo program allows you to run a simple static website and access it using a web browser via HTTP or HTTPS protocols.

The TclHttpServer component is built based on the Clever Internet Suite library and uses the fast and stable classes: TclTcpServer, TclUserConnection, and TclThreadPool.

## Read Articles
[Write your own HTTPS Server in Delphi](https://www.clevercomponents.com/articles/article050/)
[Multithreaded HTTP Server in Delphi](https://www.clevercomponents.com/articles/article044/)
