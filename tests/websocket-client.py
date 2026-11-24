"""Small independent RFC 6455 peer; no WebSocket libraries or Autobahn."""
import base64
import hashlib
import json
import os
import socket
import ssl
import struct
import sys

PORT = int(sys.argv[1])
TLS = len(sys.argv) > 2 and sys.argv[2] == "tls"

def frame(opcode, payload=b"", final=True, masked=True):
    if isinstance(payload, str):
        payload = payload.encode()
    size = len(payload)
    header = bytes([(128 if final else 0) | opcode])
    maskbit = 128 if masked else 0
    if size < 126:
        header += bytes([maskbit | size])
    elif size < 65536:
        header += bytes([maskbit | 126]) + struct.pack("!H", size)
    else:
        header += bytes([maskbit | 127]) + struct.pack("!Q", size)
    if not masked:
        return header + payload
    mask = b"\x01\x02\x03\x04"
    return header + mask + bytes(b ^ mask[i % 4] for i, b in enumerate(payload))

class Peer:
    def __init__(self, path="/raw", extra=b"", origin="https://example.test", version="13"):
        self.socket = socket.create_connection(("127.0.0.1", PORT), timeout=5)
        if TLS:
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
            context.check_hostname = False
            context.verify_mode = ssl.CERT_NONE  # Disposable test certificate only.
            self.socket = context.wrap_socket(self.socket, server_hostname="localhost")
        self.buffer = b""
        key = base64.b64encode(os.urandom(16)).decode()
        request = (f"GET {path} HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive, Upgrade\r\n"
                   f"Upgrade: websocket\r\nOrigin: {origin}\r\nSec-WebSocket-Version: {version}\r\n"
                   f"Sec-WebSocket-Key: {key}\r\n\r\n").encode()
        self.socket.sendall(request + extra)
        while b"\r\n\r\n" not in self.buffer:
            data = self.socket.recv(4096)
            assert data, "EOF before handshake"
            self.buffer += data
        self.headers, self.buffer = self.buffer.split(b"\r\n\r\n", 1)
        self.status = int(self.headers.split()[1])
        if self.status == 101:
            expected = base64.b64encode(hashlib.sha1((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode()).digest())
            assert expected in self.headers
            assert b"connection: close" not in self.headers.lower()
            assert b"content-length" not in self.headers.lower()
    def read(self, count):
        while len(self.buffer) < count:
            data = self.socket.recv(65536)
            assert data, "Unexpected EOF"
            self.buffer += data
        out, self.buffer = self.buffer[:count], self.buffer[count:]
        return out
    def receive(self):
        first, second = self.read(2)
        assert first & 128 and not first & 112, "Unexpected fragmentation/extension"
        assert not second & 128, "Server frames must be unmasked"
        size = second & 127
        if size == 126:
            size = struct.unpack("!H", self.read(2))[0]
        elif size == 127:
            size = struct.unpack("!Q", self.read(8))[0]
        return first & 15, self.read(size)
    def send(self, opcode, data=b"", final=True):
        self.socket.sendall(frame(opcode, data, final))
    def close(self):
        self.send(8, struct.pack("!H", 1000) + b"bye")
        opcode, payload = self.receive()
        assert opcode == 8 and payload[:2] == struct.pack("!H", 1000)
        self.socket.close()

# Frames already buffered behind the HTTP header must survive the handoff.
p = Peer(extra=frame(1, "early"))
assert p.status == 101
assert p.receive() == (1, b"ready")
assert p.receive() == (1, b"early")
for payload in (b"", b"x" * 126, bytes(range(256)) * 256):
    p.send(2, payload)
    assert p.receive() == (2, payload)
# UTF-8 code point split across fragments, with an interleaved control frame.
p.send(1, b"\xf0\x9f", False)
p.send(9, b"probe")
assert p.receive() == (10, b"probe")
p.send(0, b"\x98\x80")
assert p.receive() == (1, "😀".encode())
p.send(1, "push")
assert p.receive() == (1, b"pushed")
p.send(1, "slow")
p.send(9, b"during-callback")
assert p.receive() == (10, b"during-callback")
assert p.receive() == (1, b"slow-done")
p.close()

p = Peer("/json")
assert p.receive() == (1, b'"ready"')
value = {"KeepCase": [True, False, None, [], {}, "€😀", 1.25]}
p.send(1, json.dumps(value))
opcode, data = p.receive()
assert opcode == 1 and json.loads(data) == value
p.close()

for path, payload, opcode, expected in (("/raw", "fail", 1, 1011),
                                      ("/json", "{bad", 1, 1007),
                                      ("/json", b"binary", 2, 1003),
                                      ("/raw", b"\xff", 1, 1007)):
    p = Peer(path)
    p.receive()
    p.send(opcode, payload)
    op, data = p.receive()
    assert op == 8 and struct.unpack("!H", data[:2])[0] == expected, (op, data, expected)
    p.socket.close()

p = Peer()
p.receive()
p.socket.sendall(frame(1, "unmasked", masked=False))
op, data = p.receive()
assert op == 8 and struct.unpack("!H", data[:2])[0] == 1002
p.socket.close()

p = Peer()
p.receive()
p.send(1, "close")
op, data = p.receive()
assert op == 8 and data == struct.pack("!H", 1000) + b"done"
p.send(8, data)
p.socket.close()

# Close deadline: a peer that does not reply cannot hold the transport open.
p = Peer()
p.receive()
p.send(1, "close")
assert p.receive()[0] == 8
assert p.socket.recv(1) == b""
p.socket.close()

# Heartbeat runs independently of callbacks; a missing matching pong closes.
p = Peer("/heartbeat")
assert p.receive() == (1, b"ready")
op, token = p.receive()
assert op == 9
p.send(10, token)
p.send(1, "alive")
assert p.receive() == (1, b"alive")
op, token = p.receive()
assert op == 9
op, data = p.receive()
assert op == 8 and struct.unpack("!H", data[:2])[0] == 1001
p.send(8, data)
p.socket.close()

for options, expected in (({"origin": "https://evil.test"}, 403), ({"version": "12"}, 426), ({"path": "/protected"}, 401)):
    p = Peer(**options)
    assert p.status == expected
    p.socket.close()
p = Peer()
p.receive()
p.send(1, "stop")
op, data = p.receive()
assert op == 8 and struct.unpack("!H", data[:2])[0] == 1001
p.send(8, data)
p.socket.close()
print("Independent WebSocket peer: upgrade, buffering, binary/text, JSON, fragmentation, ping, push, errors and close passed.")
