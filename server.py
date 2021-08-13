import os
import http.server
import socketserver

from http import HTTPStatus

PORT = 8000
ROOT_DIR = "_build/default/pages/"


class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, request, client_address, server):
        super().__init__(request, client_address, server, directory=ROOT_DIR)

    def do_GET(self):
        if self.path.endswith("/"):
            with open(ROOT_DIR + "index.html", "rb") as f:
                fs = os.fstat(f.fileno())
                self.send_response(HTTPStatus.OK)
                self.send_header("Content-type", "text/html")
                self.send_header("Content-Length", str(fs[6]))
                self.send_header("Last-Modified", self.date_time_string(fs.st_mtime))
                self.end_headers()
                self.copyfile(f, self.wfile)
        else:
            words = self.path.split("/")
            self.path = "/" + "/".join(words[2:])
            super().do_GET()


if __name__ == "__main__":
    httpd = http.server.HTTPServer(("", PORT), Handler)
    httpd.serve_forever()
